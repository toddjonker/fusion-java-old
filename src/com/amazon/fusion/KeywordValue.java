// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Base class for syntactic forms
 * (as opposed to {@linkplain FunctionValue functions}).
 */
abstract class KeywordValue
    extends NamedValue
{
    protected final String myBodyPattern;
    protected final String myDoc;

    KeywordValue(String bodyPattern, String doc)
    {
        myBodyPattern = bodyPattern;
        myDoc = doc;
    }

    @Override
    abstract FusionValue invoke(Evaluator eval,
                                Environment env,
                                IonSexp expr)
        throws FusionException;


    @Override
    public final void write(Appendable out)
        throws IOException
    {
        out.append("/* Keyword ");
        IonTextUtils.printQuotedSymbol(out, getEffectiveName());
        out.append(" */");
    }

    @Override
    void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[SYNTAX]  (");
        out.append(getEffectiveName());
        if (myBodyPattern != null)
        {
            out.append(' ');
            out.append(myBodyPattern);
        }
        out.append(")\n\n");
        out.append(myDoc);
        out.append('\n');
    }
}
