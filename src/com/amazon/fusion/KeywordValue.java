// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.io.PrintWriter;


abstract class KeywordValue
    extends FusionValue
{
    protected final String myKeyword;
    
    KeywordValue(String keyword)
    {
        myKeyword = keyword;
    }
    
    @Override
    abstract FusionValue invoke(Evaluator eval, 
                                Environment context, 
                                IonSexp expression);

    @Override
    final void print(PrintWriter out)
    {
        out.print("// Keyword ");
        try
        {
            IonTextUtils.printQuotedSymbol(out, myKeyword);
        }
        catch (IOException e)
        {
            throw new IllegalStateException("Shouldn't happen", e);
        }
        out.println();
    }
}
