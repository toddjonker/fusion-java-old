// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleDoc.buildDocTree;
import com.amazon.ion.Timestamp;
import com.petebevin.markdown.MarkdownProcessor;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

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
        writeHtmlTree(outputDir, doc);

        System.out.println("DONE rendering docs to " + outputDir + " at "
                           + Timestamp.now());
    }


    private static void writeHtmlTree(File outputDir, ModuleDoc doc)
        throws IOException
    {
        String name = doc.baseName();
        if (name != null)
        {
            File outputFile = new File(outputDir, name + ".html");
            writeHtmlFile(outputFile, doc);
            outputDir = new File(outputDir, name);
        }

        Collection<ModuleDoc> submodules = doc.submodules();
        for (ModuleDoc submodule : submodules)
        {
            writeHtmlTree(outputDir, submodule);
        }
    }


    private static void writeHtmlFile(File out, ModuleDoc doc)
        throws IOException
    {
        out.getParentFile().mkdirs();

        FileWriter fw = new FileWriter(out);
        try
        {
            renderModule(fw, doc);
        }
        finally
        {
            fw.close();
        }
    }


    private static void renderModule(Appendable out, ModuleDoc doc)
        throws IOException
    {
        renderHead(out, doc);

        renderHeader1(out, "Module " + doc.myPath);

        renderModuleIntro(out, doc);

        renderSubmoduleLinks(out, doc);

        String[] names = doc.sortedExportedNames();

        renderBindings(out, doc, names);
    }


    private static final String STYLE =
        "<style type='text/css'>" +
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
        " .oneliner p {" +       // Markdown uses <p> but we don't want a break
        "    display: inline;" +
        " }" +
        "</style>\n";

    private static void renderHead(Appendable out, ModuleDoc doc)
        throws IOException
    {
        out.append("<head>");
        out.append("<title>");
        out.append(escape(doc.myPath));
        out.append("</title>\n");
        out.append(STYLE);
        out.append("</head>\n");
    }


    private static void renderModuleIntro(Appendable out, ModuleDoc doc)
        throws IOException
    {
        if (doc.myIntroDocs == null) return;

        String html = markdown(doc.myIntroDocs);
        out.append(html);
    }

    private static void renderSubmoduleLinks(Appendable out, ModuleDoc doc)
        throws IOException
    {
        Map<String, ModuleDoc> submodules = doc.submoduleMap();
        if (submodules == null) return;

        renderHeader2(out, "Submodules");

        String superModuleName = escape(doc.baseName());

        String[] names = submodules.keySet().toArray(new String[0]);
        Arrays.sort(names);

        out.append("<ul>");
        for (String name : names)
        {
            name = escape(name);
            out.append("<li><a href='");
            out.append(superModuleName);
            out.append('/');
            out.append(name);
            out.append(".html'>");
            out.append(name);
            out.append("</a>");

            ModuleDoc sub = submodules.get(name);
            String oneLiner = sub.oneLiner();
            if (oneLiner != null)
            {
                oneLiner = markdown(oneLiner);
                out.append(" &ndash; <span class='oneliner'>");
                out.append(oneLiner);
                out.append("</span>");
            }
            out.append("</li>\n");
        }
        out.append("</ul>\n");
    }


    private static void renderBindingIndex(Appendable out,
                                           ModuleDoc doc,
                                           String[] names)
        throws IOException
    {
        if (names.length == 0) return;

        out.append("<blockquote>");
        for (String name : names)
        {
            name = escape(name);
            out.append("<a href='#");
            out.append(name);
            out.append("'><code>");
            out.append(name);
            out.append("</code></a>&nbsp;&nbsp;\n");
        }
        out.append("</blockquote>\n");
    }


    private static void renderBindings(Appendable out,
                                       ModuleDoc doc,
                                       String[] names)
        throws IOException
    {
        Map<String, BindingDoc> bindings = doc.bindingMap();
        if (bindings == null) return;

        renderHeader2(out, "Exported Bindings");

        renderBindingIndex(out, doc, names);

        for (String name : names)
        {
            // May be null:
            BindingDoc binding = bindings.get(name);
            renderBinding(out, name, binding);
        }
    }


    private static void renderBinding(Appendable out, String name,
                                      BindingDoc doc)
        throws IOException
    {
        name = escape(name);

        out.append("<span class='binding'><span class='bound'><a name='");
        out.append(name);
        out.append("'>");
        out.append(name);
        out.append("</a></span>");   // binding span is still open

        if (doc == null)
        {
            out.append("</span>\n"); // binding
            out.append("<p>No documentation available.<p>\n\n");
        }
        else
        {
            if (doc.getKind() != null)
            {
                out.append("<span class='kind'>");
                // Using enum toString() allows display name to be changed
                out.append(doc.getKind().toString().toLowerCase());
                out.append("</span>\n");
            }
            out.append("</span>\n"); // binding

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

            out.append(markdown(buf.toString()));
        }
    }


    private static void renderHeader1(Appendable out, String text)
        throws IOException
    {
        out.append("<h1>");
        out.append(escape(text));
        out.append("</h1>\n");
    }

    private static void renderHeader2(Appendable out, String text)
        throws IOException
    {
        out.append("<h2>");
        out.append(escape(text));
        out.append("</h2>\n");
    }


    private static String escape(String text)
    {
        text = text.replace("&", "&amp;");
        text = text.replace("<", "&lt;");
        text = text.replace(">", "&gt;");
        text = text.replace("\"", "&quot;");
        text = text.replace("\'", "&apos;");
        return text;
    }


    private static String markdown(String text)
    {
        return new MarkdownProcessor().markdown(text);
    }
}
