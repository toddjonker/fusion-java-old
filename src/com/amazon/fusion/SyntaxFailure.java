// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Indicates a compile-time syntax error.
 */
@SuppressWarnings("serial")
public class SyntaxFailure
    extends FusionException
{
    private final String myName;
    private final IonValue[] myExprs;

    SyntaxFailure(String name, String message, IonValue... exprs)
    {
        super(message);
        myName = name;
        myExprs = exprs;
    }

    @Override
    public String getMessage()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            out.append("Bad syntax for ");
            IonTextUtils.printQuotedSymbol(out, myName);
            out.append(": ");
            out.append(super.getMessage());

            if (myExprs.length != 0)
            {
                out.append("\nSources:");
                for (IonValue expr : myExprs)
                {
                    out.append("\n  ");
                    FusionUtils.writeIon(out, expr);
                }
            }
        }
        catch (IOException e) {}
        return out.toString();
    }
}
