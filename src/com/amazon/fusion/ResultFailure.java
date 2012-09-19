// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import static com.amazon.fusion.FusionValue.write;
import java.io.IOException;

/**
 * Indicates a contractual failure of a result from some computation.
 */
@SuppressWarnings("serial")
final class ResultFailure
    extends ContractFailure
{
    private final String     myName;
    private final String     myExpectation;
    private final int        myBadPos;
    private final Object[]   myActuals;


    ResultFailure(String name, String expectation, Object actuals)
    {
        super("result failure");
        assert name != null && actuals != null;

        myName = name;
        myExpectation = expectation;
        myBadPos = -1;
        myActuals = new Object[]{ actuals };
    }

    /**
     *
     * @param name
     * @param expectation
     * @param badPos is zero-based
     * @param actuals
     */
    ResultFailure(String name, String expectation,
                  int badPos, Object[] actuals)
    {
        super("result failure");
        assert name != null && actuals.length != 0;
        assert 0 <= badPos && badPos < actuals.length;

        myName = name;
        myExpectation = expectation;
        myBadPos = badPos;
        myActuals = actuals;
    }


    @Override
    public String getMessage()
    {
        int actualsLen = myActuals.length;

        StringBuilder b = new StringBuilder();
        try {
            b.append(myName);
            b.append(" expects ");
            b.append(myExpectation);
            b.append(", received ");
            write(b, myActuals[actualsLen == 1 ? 0 : myBadPos]);

            if (0 <= myBadPos && 1 < actualsLen)
            {
                b.append("\n as ");
                writeFriendlyIndex(b, myBadPos);
                b.append(" result");
            }

            if (1 < actualsLen)
            {
                b.append("\n Other results were:");
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
