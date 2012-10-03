// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorSize;
import static com.amazon.fusion.FusionVector.unsafeVectorSubseq;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;

final class IonSubseqProc
    extends Procedure
{
    IonSubseqProc()
    {
        //    "                                                                               |
        super("Returns a new sequence holding the elements from SEQUENCE starting at index\n" +
              "FROM (inclusive) and ending before index TO.\n" +
              "If FROM is beyond the end of the SEQUENCE, or is not less than TO, the result\n" +
              "is an empty sequence.",
              "sequence", "from", "to");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonList sequence = null;
        int size;

        boolean seqIsVector = isVector(eval, args[0]);
        if (seqIsVector)
        {
            size = unsafeVectorSize(eval, args[0]);
        }
        else
        {
            sequence = checkIonListArg(0, args);
            size = sequence.size();
        }

        int from = checkIntArg(1, args);
        int to   = checkIntArg(2, args);

        if (to < from)
        {
            throw new ArgTypeFailure(this, "'from' less than 'to'",
                                     1, args);
        }
        if (size < to)
        {
            throw new ArgTypeFailure(this, "'to' less than (size sequence)",
                                     2, args);
        }

        assert from <= to && to <= size;

        if (from == 0 && to == size) return args[0];

        if (seqIsVector)
        {
            int len = to - from;
            return unsafeVectorSubseq(eval, args[0], from, len);
        }

        IonSystem system = eval.getSystem();
        IonSequence result = (IonSequence) system.newNull(sequence.getType());
        result.clear();

        for (int i = from; i < to; i++)
        {
            IonValue e = sequence.get(i);
            e = system.clone(e);
            result.add(e);
        }

        return eval.inject(result);
    }
}
