// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;

/**
 *
 */
class DotFunction
    extends FunctionValue
{
    DotFunction()
    {
        //    "                                                                               |
        super("Traverses down through an Ion data structure.\n" +
              "VALUE must be Ion data, either a struct or a sequence (list/sexp).\n" +
              "Each PART must be a string, symbol, or int, to denote either a struct's\n" +
              "field-name or a sequence's index.",
              "value", "part", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonValue value = ((DomValue) args[0]).getDom(); // not null

        for (int i = 1; i < args.length; i++)
        {
            IonValue partValue = ((DomValue) args[i]).getDom();
            switch (partValue.getType())
            {
                case STRING:
                case SYMBOL:
                {
                    IonText nameDom = (IonText) partValue;
                    // TODO check for null.string
                    IonStruct s = (IonStruct) value;
                    value = s.get(nameDom.stringValue());
                    break;
                }

                case INT:
                {
                    IonInt indexDom = (IonInt) partValue;
                    // TODO check for null.int
                    IonSequence s = (IonSequence) value;
                    long index = indexDom.longValue();
                    if (s.size() <= index)
                    {
                        return UNDEF;
                    }
                    value = s.get((int) index);
                    break;
                }

                // TODO handle bogus types
            }

            if (value == null) return UNDEF;
        }

        return new DomValue(value);
    }

}
