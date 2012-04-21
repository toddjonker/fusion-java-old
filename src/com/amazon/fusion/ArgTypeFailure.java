// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Indicates a failure applying a procedure with the wrong type of argument.
 */
@SuppressWarnings("serial")
final class ArgTypeFailure
    extends ContractFailure
{
    private final FunctionValue myProc;
    private final String myExpectedType;
    private final int myArgNum;
    private final FusionValue[] myActuals;

    /**
     * @param procName must not be null or empty
     */
    ArgTypeFailure(FunctionValue proc, String expectedType,
                   int argNum, FusionValue... actuals)
    {
        super("arg type failure");
        assert proc != null && actuals != null;
        myProc = proc;
        myExpectedType = expectedType;
        myArgNum = argNum;
        myActuals = actuals;
    }

    /**
     * Writes a zero-based index as a one-based position like
     * "1st", "12th, or "23rd".
     *
     * @param out
     * @param i
     * @throws IOException
     */
    private void writeFriendlyIndex(Appendable out, int i)
        throws IOException
    {
        i++;
        out.append(Integer.toString(i));
        String suffix = friendlySuffix(i);
        out.append(suffix);
    }

    private String friendlySuffix(int i)
    {
        int lastDigit = i % 10;
        if (lastDigit == 1 && i != 11)
        {
            return "st";
        }
        if (lastDigit == 2 && i != 12)
        {
            return "nd";
        }
        if (lastDigit == 3 && i != 13)
        {
            return "rd";
        }
        return "th";
    }

    @Override
    public String getMessage()
    {
        StringBuilder b = new StringBuilder();
        try {
            myProc.identify(b);
            b.append(" expects type ");
            b.append(myExpectedType);
            b.append(" as ");
            writeFriendlyIndex(b, myArgNum);
            b.append(" argument, given ");
            myActuals[myArgNum].write(b);

            if (myActuals.length != 1)
            {
                b.append("\nOther arguments were:");
                for (int i = 0; i < myActuals.length; i++)
                {
                    if (i != myArgNum)
                    {
                        b.append("\n  ");
                        myActuals[i].write(b);
                    }
                }
            }
        }
        catch (IOException e) {}
        return b.toString();
    }
}
