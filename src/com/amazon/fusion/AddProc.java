// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.cloneIfContained;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorAdd;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;

final class AddProc
    extends Procedure
{
    AddProc()
    {
        //    "                                                                               |
        super("Adds the Ion DATUM to the end of the SEQUENCE (Ion list or sexp).",
              "sequence", "datum");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        boolean seqIsVector = isVector(eval, args[0]);
        if (seqIsVector)
        {
            return unsafeVectorAdd(eval, args[0], args[1]);
        }

        IonValue seq = FusionValue.castToIonValueMaybe(args[0]);
        if (seq instanceof IonList)
        {
            // TODO FUSION-87 this makes extra copies when converting IonList
            Object vector = coerceListArg(eval, 0, args);
            return unsafeVectorAdd(eval, vector, args[1]);
        }

        if (! (seq instanceof IonSequence))
        {
            throw argFailure("Ion sequence", 0, args);
        }

        IonValue value = eval.convertToIonValueMaybe(args[1]);
        if (value == null)
        {
            throw new ArgTypeFailure(this, "Ion value", 1, args);
        }

        value = cloneIfContained(value);
        ((IonSequence) seq).add(value);

        return seq;
    }
}
