// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.writeMany;
import java.io.IOException;


/**
 * Indicates a failure applying a procedure with the wrong number of arguments.
 */
@SuppressWarnings("serial")
final class ArityFailure
    extends ContractFailure
{
    enum Variability { EXACT, AT_LEAST }

    private final Procedure myProc;
    private final int myArity;
    private final Variability myVariability;
    private final Object[] myActuals;

    /**
     * @param proc must not be null
     */
    ArityFailure(Procedure proc, int arity, Variability variability,
                 Object... actuals)
    {
        super("arity failure");
        assert proc != null && actuals != null;
        myProc = proc;
        myArity = arity;
        myVariability = variability;
        myActuals = actuals;
    }

    @Override
    public void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        myProc.identify(out);
        out.append(" expects ");
        if (myVariability == Variability.AT_LEAST)
        {
            out.append("at least ");
        }
        out.append(Integer.toString(myArity));
        out.append(" argument");
        if (myArity > 1) out.append("s");
        out.append(", given ");
        out.append(Integer.toString(myActuals.length));
        if (myActuals.length != 0)
        {
            out.append(":\n  ");
            writeMany(eval, out, myActuals, "\n  ");
        }
    }
}
