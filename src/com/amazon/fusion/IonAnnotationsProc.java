// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;

final class IonAnnotationsProc
    extends Procedure1
{
    IonAnnotationsProc()
    {
        //    "                                                                               |
        super("Returns a non-null list of strings containing the user type annotations on the\n" +
              "VALUE.",
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
            IonSystem system = eval.getSystem();

            String[] anns = value.getTypeAnnotations();
            int length = anns.length;
            if (length != 0)
            {
                result = new Object[length];
                for (int i = 0; i < length; i++)
                {
                    result[i] = system.newString(anns[i]);
                }
            }
        }

        return eval.inject(result);
    }
}
