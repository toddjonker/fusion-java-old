// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 *
 */
class MakeSexpProc
    extends Procedure
{
    MakeSexpProc()
    {
        //    "                                                                               |
        super("Creates a new S-expression with the given VALUEs. Each VALUE must be Ion data.",
              "value", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        final int len = args.length;
        IonValue[] values = new IonValue[len];

        for (int i = 0; i < len; i++)
        {
            IonValue iv = checkIonArg(i, args);
            iv = FusionUtils.cloneIfContained(iv);
            values[i] = iv;
        }

        IonSexp sexp = eval.getSystem().newSexp(values);
        return new DomValue(sexp);
    }
}
