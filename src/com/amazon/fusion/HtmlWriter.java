// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

class HtmlWriter
    implements AutoCloseable
{
    private final Writer myOut;

    HtmlWriter(File outputFile)
        throws IOException
    {
        outputFile.getParentFile().mkdirs();

        myOut = new FileWriter(outputFile);
    }

    @Override
    public void close()
        throws IOException
    {
        myOut.close();
    }


    final void append(char escapedContent)
        throws IOException
    {
        myOut.append(escapedContent);
    }

    final void append(String escapedContent)
        throws IOException
    {
        myOut.append(escapedContent);
    }


    final String escapeString(String text)
        throws IOException
    {
        text = text.replace("&", "&amp;");
        text = text.replace("<", "&lt;");
        text = text.replace(">", "&gt;");
        text = text.replace("\"", "&quot;");
        text = text.replace("\'", "&apos;");
        return text;
    }

    final void escape(String text)
        throws IOException
    {
        text = escapeString(text);
        myOut.append(text);
    }


    /**
     * @param style may be null
     */
    void renderHead(String title, String baseUrl, String style)
        throws IOException
    {
        myOut.append("<head>");

        if (baseUrl != null)
        {
            myOut.append("<base href='");
            escape(baseUrl);
            myOut.append("'>");
        }

        myOut.append("<title>");
        escape(title);
        myOut.append("</title>\n");
        if (style != null)
        {
            myOut.append("<link href='");
            escape(style);
            myOut.append("' rel='stylesheet' type='text/css'></link>\n");
        }
        
        myOut.append("</head>\n");
    }


    final void renderHeader1(String text)
        throws IOException
    {
        myOut.append("<h1>");
        escape(text);
        myOut.append("</h1>\n");
    }

    final void renderHeader2(String text)
        throws IOException
    {
        myOut.append("<h2>");
        escape(text);
        myOut.append("</h2>\n");
    }


    /**
     * Renders a link to a module, using the given link text.
     */
    final void linkToModule(ModuleIdentity id, String escapedLinkText)
        throws IOException
    {
        String escapedId = escapeString(id.absolutePath());

        append("<a href='.");
        append(escapedId);     // starts with a slash
        append(".html'>");
        append(escapedLinkText);
        append("</a>");
    }


    /**
     * Renders a link to a binding in a module, using the binding name
     * as the link text.
     */
    final void linkToBindingAsName(ModuleIdentity id,
                                   String escapedName)
        throws IOException
    {
        String escapedId = escapeString(id.absolutePath());

        append("<a href='.");
        append(escapedId);     // starts with a slash
        append(".html#");
        append(escapedName);
        append("'><code>");
        append(escapedName);
        append("</code></a>");
    }


    /**
     * Renders a link to a binding in a module, using the full module path
     * as the link text.
     */
    final void linkToBindingAsModulePath(ModuleIdentity id,
                                         String escapedName)
        throws IOException
    {
        String escapedId = escapeString(id.absolutePath());

        append("<a href='.");
        append(escapedId);     // starts with a slash
        append(".html#");
        append(escapedName);
        append("'>");
        append(escapedId);
        append("</a>");
    }
}
