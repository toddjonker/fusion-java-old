// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;
import org.junit.Test;

public class FusionValueTest
    extends CoreTestCase
{
    // TODO this isn't currently testing the case where the FV's system
    // matches the given system... because there's no API for declaring
    // the FusionRuntime's IonSystem.

    @Test
    public void testIonValue()
        throws Exception
    {
        Object fv = eval("12");
        IonValue iv = FusionValue.toIonValue(fv);
        assertEquals(12, ((IonInt)iv).intValue());
        iv = FusionValue.toIonValue(fv, system());
        assertEquals(12, ((IonInt)iv).intValue());

        fv = eval("(lambda () 12)");
        assertSame(null, FusionValue.toIonValue(fv));
        assertSame(null, FusionValue.toIonValue(fv, system()));

        fv = eval("(. [12] 0)");
        iv = FusionValue.toIonValue(fv);
        assertEquals(12, ((IonInt)iv).intValue());
        iv = FusionValue.toIonValue(fv, system());
        assertSame(null, iv.getContainer());
        assertEquals(12, ((IonInt)iv).intValue());


        fv = eval("(quote 12)");
        iv = FusionValue.toIonValue(fv);
        assertEquals(12, ((IonInt)iv).intValue());
        iv = FusionValue.toIonValue(fv, system());
        assertSame(null, iv.getContainer());
        assertEquals(12, ((IonInt)iv).intValue());
    }
}
