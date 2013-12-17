// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionText.checkNonEmptyTextArg;


final class AnnotateProc
    extends Procedure
{
    AnnotateProc()
    {
        super("... doesn't mutate the value ...",
              "value", "symbol", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        int arity = args.length;

        String[] annotations = new String[arity - 1];
        for (int i = 0; i < arity - 1; i++)
        {
            String a = checkNonEmptyTextArg(eval, this, i+1, args);
            annotations[i] = a;
        }

        Object target = args[0];

        if (target instanceof BaseValue)
        {
            Object r = ((BaseValue) target).annotate(eval, annotations);
            if (r != null) return r;
        }

        throw argFailure("annotatable type", 0, args);
    }
}
