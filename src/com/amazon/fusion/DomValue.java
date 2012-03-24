// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 * A {@link FusionValue} that contains an {@link IonValue}.
 */
class DomValue
    extends FusionValue
{
    private final IonValue myDom;

    DomValue(IonValue dom)
    {
        myDom = dom;
    }


    @Override
    boolean isTruthy()
    {
        return (myDom.getType() == IonType.BOOL
                && ! myDom.isNullValue()
                && ((IonBool) myDom).booleanValue());
    }


    @Override
    IonValue getDom()
    {
        return myDom;
    }

    @Override
    void print(Writer out)
        throws IOException
    {
        out.write(myDom.toString());
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        throw new IonException("not invokable: " + myDom);
    }
}
