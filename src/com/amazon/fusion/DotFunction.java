// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class DotFunction
    extends FunctionValue
{
    @Override
    void print(Writer out) throws IOException
    {
        out.write("// Function '.'\n");
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        out.write("(. VALUE PART ...)\n\n");
        out.write("Traverses down through an Ion data structure.\n" +
                  "VALUE must be Ion data, either a struct or a sequence (list/sexp).\n" +
                  "Each PART must be either a string or an int, to denote either a struct's\n" +
                  "field-name or a sequence's index.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonValue value = ((DomValue) args[0]).getDom();

        for (int i = 1; i < args.length; i++)
        {
            IonValue partValue = ((DomValue) args[i]).getDom();
            switch (partValue.getType())
            {
                case STRING:
                {
                    IonString nameDom = (IonString) partValue;
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
                    // TODO check for out-of-range index
                    value = s.get(indexDom.intValue());
                    break;
                }

                // TODO handle bogus types
            }
        }

        return new DomValue(value);
    }

}
