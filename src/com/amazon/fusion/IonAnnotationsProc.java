// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVector.immutableVector;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorAnnotationStrings;
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
        String[] anns = EMPTY_STRING_ARRAY;

        if (isVector(eval, arg))
        {
            anns = unsafeVectorAnnotationStrings(eval, arg);
        }
        else
        {
            IonValue value = castToIonValueMaybe(arg);

            if (value != null)
            {
                anns = value.getTypeAnnotations();
            }
        }

        Object[] result = EMPTY_OBJECT_ARRAY;
        int length = anns.length;
        if (length != 0)
        {
            result = new Object[length];
            for (int i = 0; i < length; i++)
            {
                result[i] = eval.newString(anns[i]);
            }
        }

        // Returning immutable vector allows us to return a shared structure
        // when possible, avoiding copies.
        return immutableVector(eval, result);
    }
}
