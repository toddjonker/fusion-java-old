// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleDocumentation.buildDocTree;
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


    public static void writeHtmlTree(FusionRuntime runtime,
                                     File outputDir,
                                     File repoDir)
        throws IOException, FusionException
    {
        ModuleDocumentation doc = buildDocTree(runtime, repoDir);
        writeHtmlTree(outputDir, doc);
    }


    private static void writeHtmlTree(File outputDir, ModuleDocumentation doc)
        throws IOException
    {
        if (doc.myName != null)
        {
            File outputFile = new File(outputDir, doc.myName + ".html");
            writeHtmlFile(outputFile, doc);
            outputDir = new File(outputDir, doc.myName);
        }

        Collection<ModuleDocumentation> submodules = doc.submodules();
        for (ModuleDocumentation submodule : submodules)
        {
            writeHtmlTree(outputDir, submodule);
        }
    }


    private static void writeHtmlFile(File out, ModuleDocumentation doc)
        throws IOException
    {
        out.getParentFile().mkdirs();

        String text = renderMarkdown(doc);

        // Convert Markdown to HTML
        text = new MarkdownProcessor().markdown(text);

        writeText(out, text);
    }


    private static String renderMarkdown(ModuleDocumentation doc)
        throws IOException
    {
        StringBuilder out = new StringBuilder();

        displayHeader1(out, "Module " + doc.myPath);

        renderSubmoduleLinks(out, doc);

        String[] names = doc.sortedExportedNames();

        renderBindingIndex(out, doc, names);
        renderBindings(out, doc, names);

        return out.toString();
    }


    private static void renderSubmoduleLinks(Appendable out,
                                             ModuleDocumentation doc)
        throws IOException
    {
        Map<String, ModuleDocumentation> submodules = doc.submoduleMap();
        if (submodules == null) return;

        displayHeader2(out, "Submodules");

        String superModuleName = doc.myName;

        String[] names = submodules.keySet().toArray(new String[0]);
        Arrays.sort(names);

        for (String name : names)
        {
            out.append("* [");
            out.append(name);
            out.append("](");
            out.append(superModuleName);
            out.append('/');
            out.append(name);
            out.append(".html)\n");
        }
    }


    private static void renderBindingIndex(Appendable out,
                                           ModuleDocumentation doc,
                                           String[] names)
        throws IOException
    {
        if (names.length == 0) return;

        displayHeader2(out, "Index");

        for (String name : names)
        {
            out.append("[`");
            out.append(name);
            out.append("`](#");
            out.append(name);
            out.append(") \n");
        }
    }


    private static void renderBindings(Appendable out,
                                       ModuleDocumentation doc,
                                       String[] names)
        throws IOException
    {
        Map<String, BindingDocumentation> bindings = doc.bindingMap();
        if (bindings == null) return;

        displayHeader2(out, "Exported Bindings");

        for (String name : names)
        {
            // May be null:
            BindingDocumentation feature = bindings.get(name);
            renderBinding(out, name, feature);
        }
    }

    private static void renderBinding(Appendable out, String name,
                                      BindingDocumentation doc)
        throws IOException
    {
        out.append("\n\n");
        out.append("<a name='");
        out.append(name);
        out.append("'><!-- --></a>\n");

        displayHeader3(out, name);
        out.append('\n');

        if (doc == null)
        {
            out.append("No documentation available.\n");
        }
        else
        {
            out.append("    ");
            out.append(doc.myUsage);
            out.append('\n');

            out.append('*');
            // Using enum toString() allows display name to be changed
            out.append(doc.myKind.toString());

            if (doc.myBody == null)
            {
                out.append("*\n");
            }
            else
            {
                out.append(":* ");
                out.append('\n');
                out.append(doc.myBody);
                out.append('\n');
            }
        }
    }

    private static void displayHeader1(Appendable out, String text)
        throws IOException
    {
        out.append("# ");
        out.append(text);
        out.append('\n');
    }

    private static void displayHeader2(Appendable out, String text)
        throws IOException
    {
        out.append("## ");
        out.append(text);
        out.append('\n');
    }

    private static void displayHeader3(Appendable out, String text)
        throws IOException
    {
        out.append("### ");
        out.append(text);
        out.append('\n');
    }


    private static void writeText(File out, String text)
        throws IOException
    {
        FileWriter fw = new FileWriter(out);
        try
        {
            fw.write(text);
        }
        finally
        {
            fw.close();
        }
    }
}
