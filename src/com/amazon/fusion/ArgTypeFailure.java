// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import static com.amazon.fusion.FusionValue.write;
import java.io.IOException;

/**
 * Indicates a failure applying a procedure with the wrong type of argument.
 */
@SuppressWarnings("serial")
final class ArgTypeFailure
    extends ContractFailure
{
    private final NamedValue myName;
    private final String     myExpectation;
    private final int        myBadPos;
    private final Object[]   myActuals;


    /**
     * @param badPos the zero-based index of the problematic argument.
     *   -1 means a specific arg isn't implicated.
     * @param actuals must not be null
     */
    ArgTypeFailure(NamedValue name, String expectation,
                   int badPos, Object[] actuals)
    {
        super("arg type failure");
        assert name != null && actuals.length != 0;
        assert badPos < actuals.length;

        myName = name;
        myExpectation = expectation;
        myBadPos = badPos;
        myActuals = actuals;
    }

    /**
     * @param badPos the index of the problematic argument.
     *   -1 means a specific arg isn't implicated.
     */
    ArgTypeFailure(NamedValue name, String expectation,
                   int badPos, Object actuals)
    {
        super("arg type failure");
        assert name != null && actuals != null;
        myName = name;
        myExpectation = expectation;
        myBadPos = badPos;
        myActuals = new Object[]{ actuals };
    }


    public int getBadPos()
    {
        return myBadPos;
    }

    @Override
    public String getMessage()
    {
        int actualsLen = myActuals.length;

        StringBuilder b = new StringBuilder();
        try
        {
            myName.identify(b);
            b.append(" expects ");
            b.append(myExpectation);

            if (0 <= myBadPos)
            {
                b.append(" as ");
                writeFriendlyIndex(b, myBadPos);
                b.append(" argument, given ");
                write(b, myActuals[actualsLen == 1 ? 0 : myBadPos]);
            }

            if (actualsLen != 1 || myBadPos < 0)
            {
                b.append(myBadPos < 0
                         ? "\nArguments were:"
                         : "\nOther arguments were:");

                for (int i = 0; i < actualsLen; i++)
                {
                    if (i != myBadPos)
                    {
                        b.append("\n  ");
                        write(b, myActuals[i]);
                    }
                }
            }
        }
        catch (IOException e) {}

        return b.toString();
    }
}
