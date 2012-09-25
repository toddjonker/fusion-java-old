// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import static com.amazon.fusion.FusionValue.write;

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



    /**
     *
     * @param name must not be null.
     * @param badPos the zero-based index of the problematic value.
     *   -1 means a specific position isn't implicated.
     * @param actual must not be null.
     */
    ResultFailure(String name, String expectation,
                  int badPos, Object actual)
    {
        super("result failure");
        assert name != null && actual != null;

        myName = name;
        myExpectation = expectation;
        myBadPos = badPos;
        myActuals = new Object[]{ actual };
    }

    /**
     *
     * @param name must not be null.
     * @param actual must not be null.
     */
    ResultFailure(String name, String expectation, Object actual)
    {
        this(name, expectation, -1, actual);
    }

    /**
     *
     * @param name
     * @param expectation
     * @param badPos the zero-based index of the problematic value.
     *   -1 means a specific position isn't implicated.
     * @param actuals must not be null or zero-length.
     */
    ResultFailure(String name, String expectation,
                  int badPos, Object[] actuals)
    {
        super("result failure");
        assert name != null && actuals.length != 0;
        assert badPos < actuals.length;

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
        b.append(myName);
        b.append(" expects ");
        b.append(myExpectation);

        if (0 <= myBadPos)
        {
            b.append(" as ");
            writeFriendlyIndex(b, myBadPos);
            b.append(" result, given ");
            write(b, myActuals[actualsLen == 1 ? 0 : myBadPos]);
        }

        if (actualsLen != 1 || myBadPos < 0)
        {
            b.append(myBadPos < 0
                     ? "\nResults were:"
                     : "\nOther results were:");

            for (int i = 0; i < actualsLen; i++)
            {
                if (i != myBadPos)
                {
                    b.append("\n  ");
                    write(b, myActuals[i]);
                }
            }
        }

        return b.toString();
    }
}
