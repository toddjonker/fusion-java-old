// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
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
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Object container = args[0];
        if (! (isVector(eval, container) || container instanceof IonContainer))
        {
            throw argFailure("container", 0, args);
        }

        IonValue target = eval.convertToIonValueMaybe(args[1]);
        if (target == null)
        {
            throw argFailure("Ion value", 1, args);
        }

        boolean contained = false;

        Iterator<?> listIterator = unsafeJavaIterate(eval, container);

        while (listIterator.hasNext())
        {
            Object ionValue = eval.convertToIonValueMaybe(listIterator.next());
            if (ionValue.equals(target))
            {
                contained = true;
                break;
            }
        }

        return eval.newBool(contained);
    }
}
