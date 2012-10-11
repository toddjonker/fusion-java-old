// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.makeImmutableVectorFrom;
import com.amazon.ion.IonValue;

final class IonAnnotationsProc
    extends Procedure1
{
    IonAnnotationsProc()
    {
        //    "                                                                               |
        super("Returns a non-null immutable vector of strings containing the user type\n" +
              "annotations on the VALUE.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        IonValue value = FusionValue.castToIonValueMaybe(arg);

        Object[] result = FusionUtils.EMPTY_OBJECT_ARRAY;

        if (value != null)
        {
            String[] anns = value.getTypeAnnotations();
            int length = anns.length;
            if (length != 0)
            {
                result = new Object[length];
                for (int i = 0; i < length; i++)
                {
                    result[i] = eval.newString(anns[i]);
                }
            }
        }

        // Returning immutable vector allows us to return a shared structure
        // when possible, avoiding copies.
        return makeImmutableVectorFrom(eval, result);
    }
}
