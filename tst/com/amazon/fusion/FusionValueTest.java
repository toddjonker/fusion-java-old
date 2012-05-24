// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
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
        FusionValue fv = eval("12");
        assertTrue(fv.isIon());
        IonValue iv = fv.ionValue(system());
        assertEquals(12, ((IonInt)iv).intValue());

        fv = eval("(lambda () 12)");
        assertFalse(fv.isIon());
        assertSame(null, fv.ionValue(system()));

        fv = eval("(. [12] 0)");
        assertTrue(fv.isIon());
        iv = fv.ionValue(system());
        assertSame(null, iv.getContainer());
        assertEquals(12, ((IonInt)iv).intValue());


        fv = eval("(quote 12)");
        assertTrue(fv.isIon());
        iv = fv.ionValue(system());
        assertSame(null, iv.getContainer());
        assertEquals(12, ((IonInt)iv).intValue());
    }
}
