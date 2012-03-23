// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.io.Writer;


abstract class KeywordValue
    extends FusionValue
{
    protected final String myKeyword;
    protected final String myBodyPattern;
    protected final String myDoc;

    KeywordValue(String keyword, String bodyPattern, String doc)
    {
        myKeyword = keyword;
        myBodyPattern = bodyPattern;
        myDoc = doc;
    }

    @Override
    abstract FusionValue invoke(Evaluator eval,
                                Environment context,
                                IonSexp expr);

    @Override
    final void print(Writer out)
        throws IOException
    {
        out.write("// Keyword ");
        try
        {
            IonTextUtils.printQuotedSymbol(out, myKeyword);
        }
        catch (IOException e)
        {
            throw new IllegalStateException("Shouldn't happen", e);
        }
        out.write('\n');
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        out.write('(');
        out.write(myKeyword);
        if (myBodyPattern != null)
        {
            out.write(' ');
            out.write(myBodyPattern);
        }
        out.write(")\n\n");
        out.write(myDoc);
        out.write('\n');
    }
}
