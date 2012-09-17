// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonContainer;
import com.amazon.ion.IonValue;
import java.util.Iterator;


/**
 * Accepts a container, such as a list or struct, as input and
 * an ion value and returns true if the the ion value is in the
 * container and false otherwise
 */
final class ContainsProc
    extends Procedure
{
    ContainsProc()
    {
        super("Accepts a container, such as a list or struct, as input and " +
              "and an ion value and returns true if the ion value is " +
              "in the container and false otherwise",
              "container", "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] arg)
        throws FusionException
    {
        checkArityExact(arg);

        IonContainer thisList = checkContainerArg(0, arg);
        IonValue test = FusionValue.toIonValue(arg[1]);

        boolean contained = false;

        Iterator<IonValue> listIterator = thisList.iterator();

        while(listIterator.hasNext())
        {
            IonValue ionValue = listIterator.next();
            if (ionValue.equals(test))
            {
                contained = true;
                break;
            }
        }

        return eval.newBool(contained);
    }
}
