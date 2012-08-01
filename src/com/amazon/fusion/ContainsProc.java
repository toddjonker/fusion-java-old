// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonContainer;
import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;
import java.util.Iterator;


/**
 * Accepts a list or struct as input and an ion value and returns true if the structure
 * contains the ion value and false otherwise
 */
class ContainsProc
    extends Procedure
{
    ContainsProc()
    {
        super("Accepts a list or struct as input and an ion value and " +
            "returns true if the structure contains the ion value and " +
            "false otherwise.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] arg)
        throws FusionException
    {
        checkArityExact(2,arg);

        //IonValue container = FusionValue.toIonValue(arg[0]);
        IonContainer container = checkContainerArg(0,arg);
        IonValue test = FusionValue.toIonValue(arg[1]);

        boolean contained = false;

        if (container instanceof IonContainer)
        {
            IonList thisList = (IonList)container;
            Iterator<IonValue> listIterator = thisList.listIterator();
            while (listIterator.hasNext())
            {
                IonValue ionValue = listIterator.next();
                if (ionValue.equals(test))
                {
                    contained = true;
                    break;
                }
            }
        } else
        {
            throw contractFailure ("Expected: IonList or IonStruct in the first arg; "+
                                   "observed: "+FusionValue.writeToString(arg[0]));
        }

        return eval.newBool(contained);
    }
}
