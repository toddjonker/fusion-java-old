// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
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

    /**
     * @param dom must not be null.
     */
    DomValue(IonValue dom)
    {
        assert dom != null;
        myDom = dom;
    }


    @Override
    boolean isTruthy()
    {
        return (myDom.getType() == IonType.BOOL
                && ! myDom.isNullValue()
                && ((IonBool) myDom).booleanValue());
    }


    /**
     * {@inheritDoc}
     *
     * @return not null.
     */
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
}
