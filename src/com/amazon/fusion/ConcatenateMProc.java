// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.immutableVector;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorConcatenateM;
import com.amazon.ion.IonList;


final class ConcatenateMProc
    extends Procedure
{
    ConcatenateMProc()
    {
        //    "                                                                               |
        super("Concatenates the `list`s, mutating the first argument when possible.  If the\n" +
              "first argument cannot be stretched, a fresh list is made, similar to the first.\n" +
              "Any argument that is `null.list` is treated as if it's `[]`.",
              "list", DOTDOTDOTPLUS);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        int arity = args.length;

        Object first = args[0];
        if (isVector(eval, first))
        {
            if (arity == 1) return first;
        }
        else
        {
            IonList iv = checkIonList(args, 0);
            if (arity == 1) return iv;

            first = immutableVector(eval, iv);
        }

        Object[] vectorArgs = new Object[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            Object arg = args[i];
            if (! isVector(eval, arg))
            {
                IonList iv = checkIonList(args, i);

                // Convert to vector to make appending easier
                arg = immutableVector(eval, iv);
            }
            vectorArgs[i - 1] = arg;
        }

        return unsafeVectorConcatenateM(eval, first, vectorArgs);
    }


    private IonList checkIonList(Object[] args, int pos)
        throws FusionException
    {
        try
        {
            return (IonList) castToIonValueMaybe(args[pos]);
        }
        catch (ClassCastException e)
        {
            throw argFailure("list or sexp", pos, args);
        }
    }
}
