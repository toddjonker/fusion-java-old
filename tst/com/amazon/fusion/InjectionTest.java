// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.isTrue;
import static com.amazon.fusion.FusionValue.isAnyNull;
import static com.amazon.fusion.FusionVoid.isVoid;
import static com.amazon.fusion.NumericsTest.VERY_BIG_INTEGER;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.Decimal;
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
import java.math.BigDecimal;
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
        TopLevel top = topLevel();

        // For each case we inject the Java type, make sure it round-trips,
        // and make sure it can be used in an expression.

        top.define("v", 22);
        Object fv = top.call("<", 22, BigInteger.valueOf(23));
        assertTrue(isTrue(top, fv));

        // Inject Byte
        top.define("v", Byte.valueOf(Byte.MAX_VALUE));
        assertEval(Byte.MAX_VALUE, "v");
        assertEval(Byte.MAX_VALUE * 3, "(* 3 v)");

        // Inject Short
        top.define("v", Short.valueOf(Short.MAX_VALUE));
        assertEval(Short.MAX_VALUE, "v");
        assertEval(Short.MAX_VALUE * 2, "(* 2 v)");

        // Inject Integer
        top.define("v", 22);
        assertEval(22, "v");
        assertEval(44, "(* 2 v)");

        // Inject Long
        top.define("v", Long.valueOf(Long.MAX_VALUE));
        assertEval(Long.MAX_VALUE, "v");
        assertEval(Long.MAX_VALUE - 5, "(- v 5)");

        // Inject BigInteger
        top.define("v", VERY_BIG_INTEGER);
        assertEval(VERY_BIG_INTEGER, "v");
        assertEval(VERY_BIG_INTEGER.multiply(BigInteger.valueOf(2)),
                   "(* 2 v)");
    }


    @Test
    public void testDecimalInjection()
        throws Exception
    {
        TopLevel top = topLevel();

        BigDecimal d = BigDecimal.valueOf(420);
        top.define("v", d);
        assertEval(d, "v");
        assertEval(d.add(d), "(+ v v)");
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

        iv = system().newInt(NumericsTest.VERY_BIG_INTEGER);
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

        Decimal negativeZero = Decimal.negativeZero(1);
        iv = system().newDecimal(negativeZero);
        testIonValueInjection(iv);
        assertEval(true, "(= 0 v)");
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
