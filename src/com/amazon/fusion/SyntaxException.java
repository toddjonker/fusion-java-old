// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWrite;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.Arrays;

/**
 * Indicates a compile-time syntax error.
 */
@SuppressWarnings("serial")
public class SyntaxException
    extends FusionException
{
    private final String myName;
    private SyntaxValue[] mySources;

    SyntaxException(String whatForm, String message, SyntaxValue... sources)
    {
        super(message);
        myName = whatForm;

        for (SyntaxValue s : sources)
        {
            if (s == null)
            {
                // Long way around to filter nulls
                mySources = SyntaxValue.EMPTY_ARRAY;
                for (SyntaxValue s2 : sources)
                {
                    if (s2 != null) addContext(s2);
                }
                return;
            }
        }

        mySources = sources;
        assert mySources[0] != null;
    }

    void addContext(SyntaxValue syntax)
    {
        assert syntax != null;
        int len = mySources.length;
        mySources = Arrays.copyOf(mySources, len + 1);
        mySources[len] = syntax;
    }


    @Override
    public void displayMessage(Evaluator eval, Appendable out)
        throws IOException
    {
        out.append("Bad syntax");
        if (myName != null)
        {
            out.append(" for ");
            IonTextUtils.printQuotedSymbol(out, myName);
        }
        out.append(": ");
        out.append(getBaseMessage());

        if (mySources.length != 0)
        {
            SourceLocation loc = mySources[0].getLocation();
            if (loc != null)
            {
                out.append("\nat ");
                loc.display(out);
            }

            out.append("\nSources:");
            for (SyntaxValue expr : mySources)
            {
                out.append("\n  ");
                safeWrite(eval, out, expr);
            }
        }
    }
}
