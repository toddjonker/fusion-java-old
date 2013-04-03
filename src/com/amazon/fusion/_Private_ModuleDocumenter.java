// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.DocIndex.buildDocIndex;
import static com.amazon.fusion.ModuleDoc.buildDocTree;
import com.amazon.ion.Timestamp;
import com.petebevin.markdown.MarkdownProcessor;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * NOT FOR APPLICATION USE
 */
public final class _Private_ModuleDocumenter
{
    private _Private_ModuleDocumenter() {}


    public static FusionRuntime standardDocumentingRuntime()
    {
        FusionRuntimeBuilder rb = FusionRuntimeBuilder.standard();
        rb.setDocumenting(true);
        return rb.build();
    }

    public static void writeHtmlTree(File outputDir,
                                     File repoDir)
        throws IOException, FusionException
    {
        FusionRuntime runtime = standardDocumentingRuntime();
        ModuleDoc doc = buildDocTree(runtime, repoDir);
        writeModuleTree(outputDir, ".", doc);

        DocIndex index = buildDocIndex(doc);
        writeIndexFile(outputDir, index);

        System.out.println("DONE rendering docs to " + outputDir + " at "
            + Timestamp.now());
    }


    private static void writeModuleTree(File outputDir, String baseUrl,
                                        ModuleDoc doc)
        throws IOException
    {
        String name = doc.baseName();
        if (name != null)
        {
            writeModuleFile(outputDir, baseUrl, doc);
            outputDir = new File(outputDir, name);
            baseUrl = baseUrl + "/..";
        }

        Collection<ModuleDoc> submodules = doc.submodules();
        for (ModuleDoc submodule : submodules)
        {
            writeModuleTree(outputDir, baseUrl, submodule);
        }
    }


    private static void writeModuleFile(File outputDir, String baseUrl,
                                        ModuleDoc doc)
        throws IOException
    {
        File outputFile = new File(outputDir, doc.baseName() + ".html");

        try (ModuleWriter writer = new ModuleWriter(outputFile, baseUrl))
        {
            writer.renderModule(doc);
        }
    }


    private static void writeIndexFile(File outputDir, DocIndex index)
        throws IOException
    {
        File outputFile = new File(outputDir, "binding-index.html");

        try (IndexWriter writer = new IndexWriter(outputFile))
        {
            writer.renderIndex(index);
        }
    }


    //========================================================================


    private static final class ModuleWriter
        extends HtmlWriter
    {
        private final String myBaseUrl;
        private final MarkdownProcessor myMarkdown = new MarkdownProcessor();

        public ModuleWriter(File outputFile, String baseUrl)
            throws IOException
        {
            super(outputFile);
            myBaseUrl = baseUrl;
        }

        private static final String STYLE =
            "<style type='text/css'>" +
            " .indexlink {" +
            "   float: right;" +
            " }" +
            " .binding {" +
            "   display: block; width: 100%;" +
            " }" +
            " .bound {" +
            "   font-size: 1.17em;" +
            "   font-weight: bold;" +
            " }" +
            " .kind {" +
            "   float: right; font-style: italic" +
            " }" +
            " .oneliner p {" +   // Markdown uses <p> but we don't want a break
            "    display: inline;" +
            " }" +
            "</style>\n";

        void renderModule(ModuleDoc doc)
            throws IOException
        {
            String modulePath = doc.myModuleId.internString();
            renderHead(modulePath, myBaseUrl, STYLE);

            append("<a class='indexlink' href='binding-index.html'>Binding Index</a>\n");

            renderHeader1("Module " + modulePath);

            renderModuleIntro(doc);

            renderSubmoduleLinks(doc);

            String[] names = doc.sortedExportedNames();

            renderBindings(doc, names);
        }


        private void renderModuleIntro(ModuleDoc doc)
            throws IOException
        {
            if (doc.myIntroDocs != null)
            {
                markdown(doc.myIntroDocs);
            }
        }

        private void renderSubmoduleLinks(ModuleDoc doc)
            throws IOException
        {
            Map<String, ModuleDoc> submodules = doc.submoduleMap();
            if (submodules == null) return;

            renderHeader2("Submodules");

            String[] names = submodules.keySet().toArray(new String[0]);
            Arrays.sort(names);

            append("<ul>");
            for (String name : names)
            {
                ModuleDoc sub = submodules.get(name);

                String escapedName = escapeString(name);
                append("<li>");
                linkToModule(sub.myModuleId, escapedName);

                String oneLiner = sub.oneLiner();
                if (oneLiner != null)
                {
                    append(" &ndash; <span class='oneliner'>");
                    markdown(oneLiner);
                    append("</span>");
                }
                append("</li>\n");
            }
            append("</ul>\n");
        }


        private void renderBindingIndex(ModuleDoc doc, String[] names)
            throws IOException
        {
            if (names.length == 0) return;

            append("<blockquote>\n");
            for (String name : names)
            {
                String escapedName = escapeString(name);
                linkToBindingAsName(doc.myModuleId, escapedName);
                append("&nbsp;&nbsp;\n");
            }
            append("</blockquote>\n");
        }


        private void renderBindings(ModuleDoc doc, String[] names)
            throws IOException
        {
            Map<String, BindingDoc> bindings = doc.bindingMap();
            if (bindings == null) return;

            renderHeader2("Exported Bindings");

            renderBindingIndex(doc, names);

            for (String name : names)
            {
                // May be null:
                BindingDoc binding = bindings.get(name);
                renderBinding(doc, name, binding);
            }
        }


        private void renderBinding(ModuleDoc moduleDoc,
                                   String name, BindingDoc doc)
            throws IOException
        {
            String escapedName = escapeString(name);

            append("\n<span class='binding'><span class='bound'><a name='");
            append(escapedName);
            append("'>");
            append(escapedName);
            append("</a></span>");   // binding span is still open

            if (doc == null)
            {
                append("</span>\n"); // binding
                append("<p>No documentation available.<p>\n\n");
            }
            else
            {
                if (doc.getKind() != null)
                {
                    append("<span class='kind'>");
                    // Using enum toString() allows display name to be changed
                    append(doc.getKind().toString().toLowerCase());
                    append("</span>\n");
                }
                append("</span>\n"); // binding

                StringBuilder buf = new StringBuilder();

                if (doc.getUsage() != null)
                {
                    buf.append("    ");
                    buf.append(doc.getUsage());
                    buf.append('\n');
                }

                if (doc.getBody() != null)
                {
                    buf.append('\n');
                    buf.append(doc.getBody());
                    buf.append('\n');
                }

                markdown(buf.toString());
                append('\n');


                ModuleIdentity[] ids =
                    doc.getProvidingModules().toArray(new ModuleIdentity[0]);
                Arrays.sort(ids);

                boolean printedOne = false;
                for (ModuleIdentity id : ids)
                {
                    if (id != moduleDoc.myModuleId)
                    {
                        // TODO filter out private modules
                        // TODO filter out file-system module identifiers

                        if (printedOne)
                        {
                            append(", ");
                        }
                        else
                        {
                            append("<p><em>Also provided by ");
                        }

                        linkToBindingAsModulePath(id, escapedName);
                        printedOne = true;
                    }
                }
                if (printedOne)
                {
                    append("</em></p>\n");
                }
            }
        }


        private void markdown(String text)
            throws IOException
        {
            String md = myMarkdown.markdown(text);
            append(md);
        }
    }


    //========================================================================


    private static final class IndexWriter
        extends HtmlWriter
    {
        public IndexWriter(File outputFile)
            throws IOException
        {
            super(outputFile);
        }


        private static final String STYLE =
            "<style type='text/css'>" +
            " td.bound {" +
            "   font-family: monospace;" +
            " }" +
            "</style>\n";

        void renderIndex(DocIndex index)
            throws IOException
        {
            renderHead("Fusion Binding Index", null, STYLE);

            append("<table>");
            for (Entry<String, Set<ModuleIdentity>> entry : index.getNameMap().entrySet())
            {
                String escapedName = escapeString(entry.getKey());
                append("<tr><td class='bound'>");
                append(escapedName);
                append("</td><td>");

                boolean printedOne = false;
                for (ModuleIdentity id : entry.getValue())
                {
                    if (printedOne)
                    {
                        append(", ");
                    }
                    linkToBindingAsModulePath(id, escapedName);
                    printedOne = true;
                }

                append("</td>\n");
            }
            append("</table>\n");
        }
    }
}
