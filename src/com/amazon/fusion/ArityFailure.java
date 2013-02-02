// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.writeMany;
import java.io.IOException;


/**
 * Indicates a failure applying a procedure with the wrong number of arguments.
 */
@SuppressWarnings("serial")
class ArityFailure
    extends ContractFailure
{
    private final Procedure myProc;
    private final int myMinArity;
    private final int myMaxArity;
    private final Object[] myActuals;


    /**
     * @param proc must not be null
     */
    ArityFailure(Procedure proc, int minArity, int maxArity,
                 Object... actuals)
    {
        super("arity failure");
        assert proc != null && actuals != null;
        assert minArity <= maxArity;
        myProc = proc;
        myMinArity = minArity;
        myMaxArity = maxArity;
        myActuals = actuals;
    }


    protected void displayArityExpectation(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        int base = myMinArity;
        if (myMaxArity == Integer.MAX_VALUE)
        {
            out.append("at least ");
            out.append(Integer.toString(myMinArity));
        }
        else
        {
            out.append(Integer.toString(myMinArity));

            if (myMinArity != myMaxArity)
            {
                out.append(" to ");
                out.append(Integer.toString(myMaxArity));
                base = myMaxArity;
            }
        }
        out.append(" argument");
        if (base != 1) out.append("s");
    }


    @Override
    public void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        myProc.identify(out);
        out.append(" expects ");
        displayArityExpectation(eval, out);
        out.append(", given ");
        out.append(Integer.toString(myActuals.length));
        if (myActuals.length != 0)
        {
            out.append(":\n  ");
            writeMany(eval, out, myActuals, "\n  ");
        }
    }
}
