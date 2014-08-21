// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion._Private_CoverageCollectorImpl.SRCLOC_COMPARE;
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
import java.util.HashMap;
import java.util.Map;

/**
 *
 */
public final class _Private_CoverageWriter
{
    private final static String CSS =
        "body { color:black }" +
        ".covered { color:green }" +
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

    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final _Private_CoverageCollectorImpl myCollector;

    private final int BUFFER_SIZE = 2048;
    private final byte[] myCopyBuffer = new byte[BUFFER_SIZE];

    private long myIonBytesRead;
    private boolean coverageState;



    private class CoverageInfoPair
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
            htmlWriter.append(total() + " expressions observed<br/>");
            htmlWriter.append(percentCovered() + "% expression coverage");
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
            html.append(Long.toString(total()));
            html.append("</span></div></div></td></tr></table>");
        }
    }


    private final CoverageInfoPair myGlobalCoverage = new CoverageInfoPair();

    private final Map<SourceName, CoverageInfoPair> myFileCoverages;


    public _Private_CoverageWriter(_Private_CoverageCollectorImpl collector)
    {
        myCollector     = collector;
        myFileCoverages = new HashMap<SourceName, CoverageInfoPair>();
    }


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

        SourceLocation[] locations = myCollector.sortedLocations(name);
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
                            myCollector.locationCovered(coverageLoc);
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


    private void renderSource(File       outputDir,
                              HtmlWriter indexHtml,
                              SourceName name)
        throws IOException
    {
        String relativeName = name.getFile().getPath().replaceAll("/", "_");
        relativeName = relativeName + ".html";

        indexHtml.append("<tr><td><a href=\"" + relativeName + "\">");

        ModuleIdentity id = name.getModuleIdentity();
        if (id != null)
        {
            indexHtml.append(id.absolutePath());
        }
        else
        {
            indexHtml.append(name.display());
        }

        indexHtml.append("</a></td>");

        try (HtmlWriter sourceHtml = new HtmlWriter(outputDir, relativeName))
        {
            renderSource(sourceHtml, name);
        }
    }


    private void renderTableHeading(HtmlWriter html, String category)
        throws IOException
    {
        html.append("<thead><tr><td class='heading'>");
        html.append(category);
        html.append("</td><td class='heading'>Expression Coverage</td></tr></thead>\n");
    }


    public void renderFullReport(File outputDir)
        throws IOException
    {
        HtmlWriter indexHtml = new HtmlWriter(outputDir, "index.html");

        indexHtml.renderHeadWithInlineCss("Fusion Code Coverage", CSS);
        indexHtml.append("<p>Report generated at ");
        indexHtml.append(Timestamp.now().toString());
        indexHtml.append("</p>\n");

        SourceName[] sortedSourceNames = myCollector.sortedNames();

        indexHtml.append("<table class='report'>\n");

        renderTableHeading(indexHtml, "Module");
        for (SourceName name : sortedSourceNames)
        {
            if (name.getModuleIdentity() != null)
            {
                renderSource(outputDir, indexHtml, name);
                CoverageInfoPair pair = myFileCoverages.get(name);
                indexHtml.append("<td>");
                pair.renderPercentageGraph(indexHtml);
                indexHtml.append("</td></tr>\n");
            }
        }

        renderTableHeading(indexHtml, "File");
        for (SourceName name : sortedSourceNames)
        {
            if (name.getModuleIdentity() == null && name.getFile() != null)
            {
                renderSource(outputDir, indexHtml, name);
                CoverageInfoPair pair = myFileCoverages.get(name);
                indexHtml.append("<td>");
                pair.renderPercentageGraph(indexHtml);
                indexHtml.append("</td></tr>\n");
            }
        }

        indexHtml.append("</table>\n<br/>\n");

        myGlobalCoverage.renderCoveragePercentage(indexHtml);

        indexHtml.close();
    }
}
