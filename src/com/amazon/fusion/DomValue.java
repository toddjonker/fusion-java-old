// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSystem;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
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
     * Returns the DOM representation of this value.
     * The {@link IonValue} will use the given factory
     * and will not have a container.
     *
     * @param factory must not be null.
     *
     * @return not null.
     */
    public IonValue ionValue(ValueFactory factory)
    {
        // TODO this isn't really the proper comparison
        if (myDom.getSystem() == factory && myDom.getContainer() == null)
        {
            return myDom;
        }

        // TODO FUSION-67 ION-125 should be able to clone via ValueFactory
        return ((IonSystem)factory).clone(myDom);
    }


    /**
     * Returns the DOM representation of a value.
     * The result may have a container!
     * <p>
     * This isn't public because I'm not convinced that the runtime should have
     * a singular IonSystem or ValueFactory.  Different subsystems may have
     * different needs, some using a lazy dom others with full materialization.
     *
     * @return not null.
     */
    IonValue ionValue()
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
}
