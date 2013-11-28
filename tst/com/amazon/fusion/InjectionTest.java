// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.isTrue;
import static com.amazon.fusion.FusionValue.isAnyNull;
import static com.amazon.fusion.FusionVoid.isVoid;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.IonBlob;
import com.amazon.ion.IonBool;
import com.amazon.ion.IonClob;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonFloat;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonNull;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigInteger;
import org.junit.Test;

/**
 *
 */
public class InjectionTest
    extends CoreTestCase
{
    @Test
    public void testNullInjection()
        throws Exception
    {
        Object fv = topLevel().call("identity", (Object) null);
        assertTrue(isVoid(topLevel(), fv));

        // Check that define() injects the given value.
        topLevel().define("v", null);
        fv = topLevel().eval("(identity v)");
        assertTrue(isVoid(topLevel(), fv));
    }


    @Test
    public void testBoolInjection()
        throws Exception
    {
        Object fv = topLevel().call("=", true, Boolean.FALSE);
        assertFalse(isTrue(topLevel(), fv));

        topLevel().define("v", true);
        fv = topLevel().eval("v");
        assertTrue(isTrue(topLevel(), fv));
    }


    @Test
    public void testIntInjection()
        throws Exception
    {
        Object fv = topLevel().call("<", 22, BigInteger.valueOf(23));
        assertTrue(isTrue(topLevel(), fv));

        // Inject Byte
        topLevel().define("v", Byte.valueOf(Byte.MAX_VALUE));
        fv = topLevel().eval("(= v " + Byte.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Short
        topLevel().define("v", Short.valueOf(Short.MAX_VALUE));
        fv = topLevel().eval("(= v " + Short.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Integer
        topLevel().define("v", 22);
        fv = topLevel().eval("(= v 22)");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Long
        topLevel().define("v", Long.valueOf(Long.MAX_VALUE));
        fv = topLevel().eval("(= v " + Long.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        // Inject IonInt
        IonInt iv = system().newInt(Long.MAX_VALUE);
        topLevel().define("v", iv);
        fv = topLevel().eval("(= v " + Long.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        iv.setTypeAnnotations("a");
        topLevel().define("v", iv);
        assertEval(iv, "v");
    }



    //========================================================================
    // Injection IonValues


    void testIonValueInjection(IonValue iv)
        throws Exception
    {
        topLevel().define("v", iv);
        assertEval(iv, "v");
        Object fv = eval("v");
        assertEquals(iv.isNullValue(), isAnyNull(topLevel(), fv));

        iv.setTypeAnnotations("a");
        topLevel().define("v", iv);
        assertEval(iv, "v");
        fv = eval("v");
        assertEquals(iv.isNullValue(), isAnyNull(topLevel(), fv));
    }


    @Test
    public void testIonNullInjection()
        throws Exception
    {
        IonNull iv = system().newNull();
        testIonValueInjection(iv);
    }


    @Test
    public void testIonBoolInjection()
        throws Exception
    {
        IonBool iv = system().newNullBool();
        testIonValueInjection(iv);

        iv = system().newBool(false);
        testIonValueInjection(iv);

        iv = system().newBool(true);
        testIonValueInjection(iv);
    }


    @Test
    public void testIonIntInjection()
        throws Exception
    {
        IonInt iv = system().newNullInt();
        testIonValueInjection(iv);

        iv = system().newInt(Long.MAX_VALUE);
        testIonValueInjection(iv);
    }


    @Test
    public void testIonFloatInjection()
        throws Exception
    {
        IonFloat iv = system().newNullFloat();
        testIonValueInjection(iv);

        iv = system().newFloat(Float.MAX_VALUE);
        testIonValueInjection(iv);
    }


    @Test
    public void testIonDecimalInjection()
        throws Exception
    {
        IonDecimal iv = system().newNullDecimal();
        testIonValueInjection(iv);

        iv = system().newDecimal(12345);
        testIonValueInjection(iv);
    }


    @Test
    public void testIonTimestampInjection()
        throws Exception
    {
        IonTimestamp iv = system().newNullTimestamp();
        testIonValueInjection(iv);

        iv = system().newTimestamp(Timestamp.forDay(2013, 12, 2));
        testIonValueInjection(iv);
    }


    @Test
    public void testIonSymbolInjection()
        throws Exception
    {
        IonSymbol iv = system().newNullSymbol();
        testIonValueInjection(iv);

        iv = system().newSymbol("vanilla soy mocha");
        testIonValueInjection(iv);
    }


    @Test
    public void testIonStringInjection()
        throws Exception
    {
        IonString iv = system().newNullString();
        testIonValueInjection(iv);

        iv = system().newString("pumpkin spice latte");
        testIonValueInjection(iv);
    }


    private static final byte[] BYTES = { 0, 1, 2, 3 };

    @Test
    public void testIonClobInjection()
        throws Exception
    {
        IonClob iv = system().newNullClob();
        testIonValueInjection(iv);

        iv = system().newClob(BYTES);
        testIonValueInjection(iv);
    }


    @Test
    public void testIonBlobInjection()
        throws Exception
    {
        IonBlob iv = system().newNullBlob();
        testIonValueInjection(iv);

        iv = system().newBlob(BYTES);
        testIonValueInjection(iv);
    }
}
