// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;

/**
 *
 */
class PlusFunction
    extends FunctionValue
{
    PlusFunction()
    {
        //    "                                                                               |
        super("Adds one or more integers.",
              "int", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonInt value = (IonInt) ((DomValue) args[0]).getDom().clone();

        for (int i = 1; i < args.length; i++)
        {
            IonValue argValue = ((DomValue) args[i]).getDom();
            switch (argValue.getType())
            {
                case INT:
                {
                    IonInt argInt = (IonInt) argValue;
                    // TODO check for null.int
                    long sum = value.longValue() + argInt.longValue();
                    value.setValue(sum);
                    break;
                }

                // TODO handle bogus types
            }
        }

        return new DomValue(value);
    }
}
