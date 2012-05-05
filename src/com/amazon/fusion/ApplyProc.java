// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;

public class ApplyProc
    extends Procedure
{
    ApplyProc()
    {
        //    "                                                                               |
        super("Calls the given PROC with arguments that are the (optional) ARGs prepended to\n" +
              "the elements of SEQUENCE. The PROC is called in tail position.\n" +
              "(apply + [1, 2])         => 3\n" +
              "(apply + 10 11 [1, 2])   => 24",
              "proc", "arg", DOTDOTDOT, "sequence");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityAtLeast(2, args);

        int arity = args.length;

        Procedure proc = checkProcArg(0, args);
        IonSequence rest = checkSequenceArg(arity - 1, args);
        int restLen = rest.size();

        int procArity = restLen + arity - 2;
        FusionValue[] procArgs = new FusionValue[procArity];

        int arg = 0;
        for (int i = 1; i < arity - 1; i++)
        {
            procArgs[arg++] = args[i];
        }

        for (IonValue dom : rest)
        {
            procArgs[arg++] = new DomValue(dom);
        }
        assert arg == procArity;

        return eval.bounceTailCall(proc, procArgs);
    }
}
