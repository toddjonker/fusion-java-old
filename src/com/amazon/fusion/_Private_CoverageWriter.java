// Copyright (c) 2014-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.CoverageDatabase.SRCLOC_COMPARE;
import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import static java.math.RoundingMode.HALF_EVEN;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonType;
import com.amazon.ion.OffsetSpan;
import com.amazon.ion.Span;
import com.amazon.ion.SpanProvider;
import com.amazon.ion.Timestamp;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 */
public final class _Private_CoverageWriter
{
    private final static String CSS =
        "body { color:black }" +
        ".uncovered { color:red }" +
        "table.percentgraph { border: 0px;font-size: 130%;margin: 0px;margin-left: auto; margin-right: 0px;" +
        "padding: 0px; cellpadding=\"0px\" cellspacing=\"0px\"}" +
        "table.percentgraph tr.percentgraph { border: 0px;margin: 0px;padding: 0px;}" +
        "table.percentgraph td.percentgraph { border: 0px;margin: 0px;padding: 0px;padding-left: 4px;}" +
        "table.percentgraph td.percentgraphright { align=\"right\"; border: 0px;margin: 0px;padding: 0px;padding-left: 4px; width=\"40\"}" +
        "div.percentgraph { background-color: #f02020;border: #808080 1px solid;height: 1.3em;margin: 0px;padding: 0px;width: 100px;}" +
        "div.percentgraph div.greenbar { background-color: #00f000;height: 1.3em;margin: 0px;padding: 0px;}" +
        "div.percentgraph div.na { background-color: #eaeaea;height: 1.3em;margin: 0px;padding: 0px;}" +
        "div.percentgraph span.text { display: block;position: absolute;text-align: center;width: 100px;}" +
        "table.report { border-collapse: collapse;width: 100%;}" +
        "table.report td { border: #d0d0d0 1px solid;}" +
        "table.report td.heading {background: #dcecff;font-weight: bold;text-align: center;}" +
        "table.report td.value { text-align: right;}" +
        "table tr td, table tr th {font-size: 68%;}" +
        "td.value table tr td {font-size: 11px;}" +
        "div.separator {height: 10px;}";


    private static class CoverageInfoPair
    {
        public long coveredExpressions;
        public long uncoveredExpressions;

        public CoverageInfoPair()
        {
            coveredExpressions   = 0;
            uncoveredExpressions = 0;
        }

        void foundExpression(boolean covered)
        {
            if (covered)
            {
                coveredExpressions++;
            }
            else
            {
                uncoveredExpressions++;
            }
        }

        public long total()
        {
            return coveredExpressions + uncoveredExpressions;
        }

        BigDecimal percentCovered()
        {
            final long total = total();

            if (total == 0) return BigDecimal.ZERO;

            BigDecimal numerator = new BigDecimal(coveredExpressions * 100);

            final BigDecimal percentCovered =
                numerator.divide(new BigDecimal(total), 2, HALF_EVEN);

            return percentCovered;
        }

        void renderCoveragePercentage(HtmlWriter htmlWriter)
            throws IOException
        {
            htmlWriter.append(percentCovered().toString());
            htmlWriter.append("% expression coverage of ");
            htmlWriter.append(Long.toString(total()));
            htmlWriter.append(" expressions observed");
        }

        void renderTotal(HtmlWriter html)
            throws IOException
        {
            html.append(Long.toString(total()));
        }

        void renderPercentageGraph(HtmlWriter html)
            throws IOException
        {
            final BigDecimal percent = percentCovered();
            final int percentIntVal = percent.intValue();

            html.append("<table class='percentgraph'>"
                          + "<tr class='percentgraph'>"
                          + "<td class='percentgraphright'>");
            html.append(Integer.toString(percentIntVal));
            html.append("%</td>");
            html.append("<td class='percentgraph'>"
                          + "<div class='percentgraph'>"
                          + "<div class='greenbar' style='width:");
            html.append(Integer.toString(percentIntVal));
            html.append("px'><span class='text'>");
            html.append(Long.toString(coveredExpressions));
            html.append("/");
            renderTotal(html);
            html.append("</span></div></div></td></tr></table>");
        }
    }

    private static class EstimatedCoverageInfoPair
        extends CoverageInfoPair
    {
        @Override
        void renderTotal(HtmlWriter html)
            throws IOException
        {
            html.append("???");
        }
    }


    private final CoverageDatabase                  myDatabase;
    private final CoverageConfiguration             myConfig;
    private final Set<ModuleIdentity>               myModules;
    private final Set<File>                         mySourceFiles;
    private final Map<ModuleIdentity, SourceName>   myNamesForModules;
    private final Map<File, SourceName>             myNamesForFiles;
    private final Map<SourceName, CoverageInfoPair> myFileCoverages;
    private final Map<SourceName, String>           myRelativeNamesForSources;

    private final CoverageInfoPair myGlobalCoverage = new CoverageInfoPair();
    private       int              myUnloadedEntries;

    // For rendering highlighted source files
    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private static final int BUFFER_SIZE = 2048;
    private final byte[] myCopyBuffer = new byte[BUFFER_SIZE];
    private long myIonBytesRead;
    private boolean coverageState;


    public _Private_CoverageWriter(File dataDir)
        throws FusionException, IOException
    {
        myDatabase = new CoverageDatabase(dataDir);
        myConfig   = new CoverageConfiguration(dataDir);

        myModules                 = new HashSet<>();
        mySourceFiles             = new HashSet<>();
        myNamesForModules         = new HashMap<>();
        myNamesForFiles           = new HashMap<>();
        myFileCoverages           = new HashMap<>();
        myRelativeNamesForSources = new HashMap<>();
    }


    //=========================================================================
    // Metrics Analysis


    private void collectSourceFiles(File dir)
    {
        String[] fileNames = dir.list();

        for (String fileName : fileNames)
        {
            File file = new File(dir, fileName);
            if (file.isDirectory())
            {
                collectSourceFiles(file);
            }
            else if (fileName.endsWith(FUSION_SOURCE_EXTENSION))
            {
                if (myConfig.fileIsSelected(file))
                {
                    mySourceFiles.add(file);
                }
            }
        }
    }


    private Path commonPrefix(Path a, Path b)
    {
        int maxLen = Math.min(a.getNameCount(), b.getNameCount());
        for (int i = 0; i < maxLen; i++)
        {
            if (! a.getName(i).equals(b.getName(i)))
            {
                maxLen = i;
            }
        }

        // Path.subpath(0, 0) is illegal
        return (maxLen != 0
                    ? a.subpath(0, maxLen)
                    : a.getFileSystem().getPath(""));
    }

    private int commonPrefixLen(Set<SourceName> sourceNames)
        throws IOException
    {
        Path prefix = null;
        for (SourceName sourceName : sourceNames)
        {
            Path path = sourceName.getFile().getCanonicalFile().toPath();
            Path parent = path.getParent();
            if (prefix == null)
            {
                prefix = parent;
            }
            else {
                prefix = commonPrefix(prefix, parent);
            }
        }
        return prefix == null ? 0 : prefix.getNameCount();
    }

    private void prepareRelativeNames()
        throws IOException
    {
        Set<SourceName> sourceNames = myDatabase.sourceNames();
        int prefixLen = commonPrefixLen(sourceNames);

        for (SourceName sourceName : sourceNames)
        {
            Path path = sourceName.getFile().getCanonicalFile().toPath();
            Path shorterPath = path.subpath(prefixLen, path.getNameCount());
            myRelativeNamesForSources.put(sourceName, shorterPath.toString());
        }
    }


    private void analyze()
        throws FusionException
    {
        Consumer<ModuleIdentity> consumer = new Consumer<ModuleIdentity>()
        {
            @Override
            public void accept(ModuleIdentity t)
            {
                myModules.add(t);
            }
        };

        for (File f : myDatabase.getRepositories())
        {
            // TODO FUSION-214 Push this into the repo impl
            File src = new File(f, "src");
            ModuleRepository repo = new FileSystemModuleRepository(src);
            repo.collectModules(myConfig.myModuleSelector, consumer);
        }

        Set<String> includedSourceDirs = myConfig.getIncludedSourceDirs();
        if (includedSourceDirs != null)
        {
            for (String s : includedSourceDirs)
            {
                File dir = new File(s);
                collectSourceFiles(dir);
            }
        }

        for (SourceLocation loc : myDatabase.locations())
        {
            SourceName sourceName = loc.getSourceName();
            if (sourceName != null)
            {
                ModuleIdentity id = sourceName.getModuleIdentity();
                if (id != null && myConfig.myModuleSelector.test(id))
                {
                    myModules.add(id);

                    SourceName prior = myNamesForModules.put(id, sourceName);
                    assert prior == null || prior == sourceName;
                }
                else
                {
                    File f = sourceName.getFile();
                    if (f != null)
                    {
                        mySourceFiles.add(f);

                        SourceName prior = myNamesForFiles.put(f, sourceName);
                        assert prior == null || prior == sourceName;
                    }
                }
            }
        }
    }


    private ModuleIdentity[] sortedModules()
    {
        ModuleIdentity[] modules = myModules.toArray(new ModuleIdentity[0]);
        Arrays.sort(modules);
        return modules;
    }

    private File[] sortedFiles()
    {
        File[] result = mySourceFiles.toArray(new File[0]);
        Arrays.sort(result);
        return result;
    }


    //=========================================================================
    // Report Rendering

    private void copySourceThroughOffset(HtmlWriter  htmlWriter,
                                         InputStream source,
                                         long        offset)
        throws IOException
    {
        long bytesToCopy = offset - myIonBytesRead;

        long bytesCopied = 0;
        while (bytesCopied < bytesToCopy)
        {
            int toRead = (int) Math.min(bytesToCopy - bytesCopied, BUFFER_SIZE);

            int bytesRead = source.read(myCopyBuffer, 0, toRead);

            if (bytesRead < 0) break; // EOF

            htmlWriter.write(myCopyBuffer, 0, bytesRead);
            bytesCopied += bytesRead;
        }

        myIonBytesRead += bytesCopied;
    }


    private void copySourceThroughCurrentOffset(HtmlWriter   htmlWriter,
                                                InputStream  source,
                                                SpanProvider spanProvider)
        throws IOException
    {
        Span span = spanProvider.currentSpan();
        OffsetSpan offsetSpan = span.asFacet(OffsetSpan.class);
        long offset = offsetSpan.getStartOffset();
        copySourceThroughOffset(htmlWriter, source, offset);
    }


    private void setCoverageState(HtmlWriter   htmlWriter,
                                  InputStream  source,
                                  SpanProvider spanProvider,
                                  boolean      covered)
        throws IOException
    {
        if (covered != coverageState)
        {
            copySourceThroughCurrentOffset(htmlWriter, source, spanProvider);

            htmlWriter.append("</span><span class='");
            if (! covered)
            {
                htmlWriter.append("un");
            }
            htmlWriter.append("covered'>");
        }

        coverageState = covered;
    }


    private void renderSource(HtmlWriter sourceHtml,
                              SourceName name)
        throws IOException
    {
        File sourceFile = name.getFile();

        sourceHtml.renderHeadWithInlineCss("Fusion Code Coverage", CSS);
        {
            sourceHtml.append("<h1>");
            ModuleIdentity id = name.getModuleIdentity();
            if (id != null)
            {
                sourceHtml.append("Module ");
                sourceHtml.append(id.absolutePath());
                sourceHtml.append("</h1>\n");

                String path = sourceFile.getAbsolutePath();
                sourceHtml.append("at ");
                sourceHtml.append(sourceHtml.escapeString(path));
            }
            else
            {
                sourceHtml.append("File ");
                sourceHtml.append(name.display());
                sourceHtml.append("</h1>\n");
            }
        }

        SourceLocation[] locations = myDatabase.sortedLocations(name);
        assert locations.length != 0;

        int locationIndex = 0;
        final CoverageInfoPair coverageInfoPair = new CoverageInfoPair();

        sourceHtml.append("\n<hr/>\n");
        sourceHtml.append("<pre>");

        try (InputStream myIonBytes = new FileInputStream(sourceFile))
        {
            myIonBytesRead = 0;

            try (IonReader ionReader =
                    mySystem.newReader(new FileInputStream(sourceFile)))
            {
                SpanProvider spanProvider =
                    ionReader.asFacet(SpanProvider.class);

                // We always start with a span so we can always end with one,
                // regardless of the data in between.
                coverageState = false;
                sourceHtml.append("<span class='uncovered'>");

                for (IonType t = ionReader.next(); t != null; )
                {
                    // Determine whether this value has been covered.
                    SourceLocation currentLoc =
                        SourceLocation.forCurrentSpan(ionReader, null);

                    SourceLocation coverageLoc = locations[locationIndex];

                    // We shouldn't skip past a known location.
                    assert SRCLOC_COMPARE.compare(currentLoc, coverageLoc) <= 0;

                    if (SRCLOC_COMPARE.compare(currentLoc, coverageLoc) == 0)
                    {
                        boolean covered =
                            myDatabase.locationCovered(coverageLoc);
                        setCoverageState(sourceHtml, myIonBytes, spanProvider,
                                         covered);
                        locationIndex++;

                        coverageInfoPair.foundExpression(covered);
                        myGlobalCoverage.foundExpression(covered);

                        if (locationIndex == locations.length) break;
                    }

                    if (IonType.isContainer(t))
                    {
                        ionReader.stepIn();
                    }

                    while ((t = ionReader.next()) == null
                           && ionReader.getDepth() != 0)
                    {
                        ionReader.stepOut();
                    }
                }

                assert locationIndex == locations.length
                    : "Not all locations were found in the source";
                assert locationIndex == coverageInfoPair.total()
                    : "Not all locations were counted";

                // Copy the rest of the Ion source.
                copySourceThroughOffset(sourceHtml, myIonBytes, Long.MAX_VALUE);

                sourceHtml.append("</span>");
            }
        }

        sourceHtml.append("</pre>\n");
        sourceHtml.append("<hr/>");
        coverageInfoPair.renderCoveragePercentage(sourceHtml);

        myFileCoverages.put(name, coverageInfoPair);
    }


    private String relativeName(SourceName name)
    {
        return myRelativeNamesForSources.get(name) + ".html";
    }


    private void renderSourceFiles(File outputDir)
        throws IOException
    {
        for (SourceName name : myDatabase.sourceNames())
        {
            String relativeName = relativeName(name);
            try (HtmlWriter sourceHtml = new HtmlWriter(outputDir, relativeName))
            {
                renderSource(sourceHtml, name);
            }
        }
    }


    private <T> void renderRows(HtmlWriter         indexHtml,
                                String             category,
                                T[]                keys,
                                Map<T, SourceName> keyToNames)
        throws IOException
    {
        int totalExpressions = 0;
        int unloadedCount = 0;

        boolean first = true;
        for (T key : keys)
        {
            if (first)
            {
                indexHtml.append("<thead><tr>");
                indexHtml.append("<td class='heading'>Expression Coverage</td>");
                indexHtml.append("<td class='heading'>");
                indexHtml.append(category);
                indexHtml.append("</td>");
                indexHtml.append("</tr></thead>\n");
                first = false;
            }

            CoverageInfoPair pair;
            SourceName name = keyToNames.get(key);
            if (name != null)
            {
                pair = myFileCoverages.get(name);

                totalExpressions += pair.total();
            }
            else // The module was never loaded!
            {
                pair = new EstimatedCoverageInfoPair();

                unloadedCount++;
            }

            indexHtml.append("<tr><td>");
            pair.renderPercentageGraph(indexHtml);
            indexHtml.append("</td><td>");

            if (name != null)
            {
                indexHtml.append("<a href=\"" + relativeName(name) + "\">");
                indexHtml.append(key.toString());
                indexHtml.append("</a>");
            }
            else
            {
                indexHtml.append(key.toString());
            }

            indexHtml.append("</td></tr>\n");
        }

        if (unloadedCount != 0)
        {
            int loadedCount = keys.length - unloadedCount;
            int average = (loadedCount == 0
                               ? 500                        // Totally made up
                               : totalExpressions / loadedCount);

            myGlobalCoverage.uncoveredExpressions += (unloadedCount * average);

            myUnloadedEntries += unloadedCount;
        }
    }


    private void renderIndex(File outputDir)
        throws IOException
    {
        try (HtmlWriter indexHtml = new HtmlWriter(outputDir, "index.html"))
        {
            indexHtml.renderHeadWithInlineCss("Fusion Code Coverage", CSS);

            indexHtml.append("<p>Report generated at ");
            indexHtml.append(Timestamp.now().toString());
            indexHtml.append("</p>\n");

            indexHtml.append("<table class='report'>\n");

            renderRows(indexHtml, "Module", sortedModules(), myNamesForModules);
            renderRows(indexHtml, "File",   sortedFiles(),   myNamesForFiles);

            indexHtml.append("</table>\n<br/>\n");

            // We can't render this earlier since the result is affected by
            // unloaded rows.
            myGlobalCoverage.renderCoveragePercentage(indexHtml);
            if (myUnloadedEntries != 0)
            {
                indexHtml.append(" (estimate since ");
                indexHtml.append(Integer.toString(myUnloadedEntries));
                indexHtml.append(" files were not loaded)");
            }
        }
    }


    public void renderFullReport(File outputDir)
        throws FusionException, IOException
    {
        analyze();

        prepareRelativeNames();

        renderSourceFiles(outputDir);
        renderIndex(outputDir);
    }
}
