// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.Arrays;

/**
 * Indicates a compile-time syntax error.
 */
@SuppressWarnings("serial")
public class SyntaxFailure
    extends FusionException
{
    private final String myName;
    private SyntaxValue[] mySources;

    SyntaxFailure(String name, String message, SyntaxValue... sources)
    {
        super(message);
        myName = name;
        mySources = sources;
    }

    void addContext(SyntaxValue syntax)
    {
        int len = mySources.length;
        mySources = Arrays.copyOf(mySources, len + 1);
        mySources[len] = syntax;
    }


    @Override
    public String getMessage()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            out.append("Bad syntax");
            if (myName != null)
            {
                out.append(" for ");
                IonTextUtils.printQuotedSymbol(out, myName);
            }
            out.append(": ");
            out.append(super.getMessage());

            if (mySources.length != 0)
            {
                out.append("\nSources:");
                for (SyntaxValue expr : mySources)
                {
                    out.append("\n  ");
                    expr.write(out);
                }
            }
        }
        catch (IOException e) {}
        return out.toString();
    }
}
