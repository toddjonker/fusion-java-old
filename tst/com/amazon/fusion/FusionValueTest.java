// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isEmptySexp;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafePairTail;
import static com.amazon.fusion.FusionValue.annotationsAsJavaStrings;
import static com.amazon.fusion.FusionValue.isAnnotated;
import static org.junit.Assert.assertArrayEquals;
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

    @Test
    public void testIsAnnotated()
        throws Exception
    {
        useTstRepo();
        TopLevel top = topLevel();
        Evaluator eval = evaluator();
        top.requireModule("/testutils");

        Object values = top.lookup("representative_ion_data");
        assertTrue(isSexp(eval, values));

        while (! isEmptySexp(eval, values))
        {
            Object value = unsafePairHead(eval, values);
            assertFalse(isAnnotated(eval, value));

            value = top.call("annotate", value, "ann");
            assertTrue(value.toString(), isAnnotated(eval, value));

            String[] anns = annotationsAsJavaStrings(eval, value);
            assertArrayEquals("annotations on " + value,
                              new String[]{ "ann" }, anns);

            values = unsafePairTail(eval, values);
        }
    }


    private boolean hasAnnotation(String expr, String annotation)
        throws Exception
    {
        Object value = eval(expr);
        return FusionValue.hasAnnotation(evaluator(), value, annotation);
    }

    @Test
    public void testHasAnnotation()
        throws Exception
    {
        useTstRepo();

        assertTrue(hasAnnotation("(quote a::null)", "a"));
        assertTrue(hasAnnotation("(quote a::b::null)", "a"));
        assertTrue(hasAnnotation("(quote a::b::null)", "b"));
        assertTrue(hasAnnotation("(quote ''::null)", ""));

        assertFalse(hasAnnotation("(quote a::null)", "x"));
        assertFalse(hasAnnotation("(quote a::b::null)", "x"));
    }
}
