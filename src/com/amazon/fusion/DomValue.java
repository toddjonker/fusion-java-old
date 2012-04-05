// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonText;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * A {@link FusionValue} that contains an {@link IonValue}.
 */
class DomValue
    extends FusionValue
    implements Writeable
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
    void display(Writer out)
        throws IOException
    {
        String text;
        if (myDom instanceof IonText)
        {
            text = ((IonText) myDom).stringValue();
        }
        else
        {
            // TODO avoid building the string
            text = myDom.toString();
        }
        out.write(text);
    }

    @Override
    public void write(IonWriter out)
    {
        myDom.writeTo(out);
    }
}
