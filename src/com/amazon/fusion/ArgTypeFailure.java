// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import java.io.IOException;

/**
 * Indicates a failure applying a procedure with the wrong type of argument.
 */
@SuppressWarnings("serial")
final class ArgTypeFailure
    extends ContractFailure
{
    private final NamedValue myName;
    private final String myExpectedType;
    private final int myArgNum;
    private final FusionValue[] myActuals;


    ArgTypeFailure(NamedValue name, String expectedType,
                   int argNum, FusionValue... actuals)
    {
        super("arg type failure");
        assert name != null && actuals != null;
        myName = name;
        myExpectedType = expectedType;
        myArgNum = argNum;
        myActuals = actuals;
    }



    @Override
    public String getMessage()
    {
        StringBuilder b = new StringBuilder();
        try {
            myName.identify(b);
            b.append(" expects ");
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
