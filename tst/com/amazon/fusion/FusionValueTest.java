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
        checkLong(12, fv);
        IonValue iv = FusionValue.copyToIonValue(fv, system());
        assertEquals(12, ((IonInt)iv).intValue());

        fv = eval("(lambda () 12)");
        assertSame(null, FusionValue.copyToIonValueMaybe(fv, system()));
    }

    @Test(expected = FusionException.class)
    public void testBadCopyToIonValue()
        throws Exception
    {
        Object fv = eval("(lambda () 12)");
        FusionValue.copyToIonValue(fv, system());
    }
}
