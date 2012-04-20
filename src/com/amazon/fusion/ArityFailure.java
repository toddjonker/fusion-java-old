// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;


/**
 *
 */
final class ArityFailure
    extends FusionException
{
    private static final long serialVersionUID = 1L;

    private final FunctionValue myProc;
    private final int myArity;
    private final FusionValue[] myActuals;

    /**
     * @param procName must not be null or empty
     */
    ArityFailure(FunctionValue proc, int arity, FusionValue... actuals)
    {
        super("arity failure");
        assert proc != null && actuals != null;
        myProc = proc;
        myArity = arity;
        myActuals = actuals;
    }

    @Override
    public String getMessage()
    {
        StringBuilder b = new StringBuilder();
        try {
            myProc.identify(b);
            b.append(" expects ");
            b.append(myArity);
            b.append(" arguments, given ");
            b.append(myActuals.length);
            if (myActuals.length != 0)
            {
                b.append(":\n  ");
                FusionValue.write(b, myActuals, "\n  ");
            }
        }
        catch (IOException e) {}
        return b.toString();
    }
}
