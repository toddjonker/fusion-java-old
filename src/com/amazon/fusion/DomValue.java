// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import java.io.IOException;

/**
 * A {@link FusionValue} that contains an {@link IonValue}.
 */
final class DomValue
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
    public void write(Appendable out)
        throws IOException
    {
        FusionUtils.writeIon(out, myDom);
    }


    @Override
    public void display(Appendable out)
        throws IOException
    {
        if (myDom instanceof IonText)
        {
            String text = ((IonText) myDom).stringValue();
            out.append(text);
        }
        else
        {
            write(out);
        }
    }

    @Override
    public void write(IonWriter out)
    {
        myDom.writeTo(out);
    }

    //========================================================================

    static IonSequence assumeSequence(FusionValue v)
    {
        // TODO error checking
        return (IonSequence) ((DomValue) v).myDom;
    }

    static IonStruct assumeStruct(FusionValue v)
    {
        // TODO error checking
        return (IonStruct) ((DomValue) v).myDom;
    }

    static IonText assumeText(FusionValue v)
    {
        // TODO error checking
        return (IonText) ((DomValue) v).myDom;
    }

    static String assumeTextContent(FusionValue v)
    {
        return assumeText(v).stringValue();
    }
}
