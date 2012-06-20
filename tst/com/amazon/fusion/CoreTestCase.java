// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.amazon.ion.IonContainer;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.junit.After;
import org.junit.Assert;

public class CoreTestCase
{
    static final String NONTERMINATING_EXPRESSION =
        "((lambda (x) (x x)) (lambda (x) (x x)))";

    /** Exprs that evaluate to Ion types. */
    private static final String[] ION_EXPRESSIONS =
    {
        "null",
        "null.bool", "true", "false",
        "null.int", "0", "1", "12345",
        "null.decimal", "0.", "0.0", "123.45",
        "null.float", "0e0", "123e45",
        "null.timestamp", "2012-04-20T16:20-07:00",
        "null.string", "\"\"", "\"text\"",
        "(quote null.symbol)", "(quote sym)",
        "null.blob", "{{}}",
        "null.clob", "{{\"\"}}",
        "null.list", "[]",
        "(quote null.sexp)", "(quote ())",
        "null.struct", "{}",
    };

    /** Exprs that evaluate to non-Ion types. */
    private static final String[] NON_ION_EXPRESSIONS =
    {
        "undef",
        "(lambda () 1)",
    };


    //========================================================================


    private IonSystem mySystem = IonSystemBuilder.standard().build();
    private FusionRuntime myRuntime = FusionRuntimeBuilder.standard().build();

    @After
    public void tearDown()
    {
        mySystem = null;
        myRuntime = null;
    }

    protected IonSystem system()
    {
        return mySystem;
    }


    //========================================================================

    List<String> allTypeExpressions()
    {
        ArrayList<String> exprs = new ArrayList<String>();
        Collections.addAll(exprs, ION_EXPRESSIONS);
        Collections.addAll(exprs, NON_ION_EXPRESSIONS);
        assert exprs.size() != 0;
        return exprs;
    }


    List<String> allIonExpressions()
        throws FusionException
    {
        ArrayList<String> exprs = new ArrayList<String>();
        Collections.addAll(exprs, ION_EXPRESSIONS);
        assert exprs.size() != 0;
        return exprs;
    }


    List<String> nonIonExpressions()
        throws FusionException
    {
        ArrayList<String> exprs = new ArrayList<String>();
        Collections.addAll(exprs, NON_ION_EXPRESSIONS);
        assert exprs.size() != 0;
        return exprs;
    }


    <T extends IonValue> List<String> nonIonExpressions(Class<T> klass)
        throws FusionException
    {
        ArrayList<String> exprs = new ArrayList<String>();
        for (String expr : allTypeExpressions())
        {
            FusionValue v = eval(expr);
            IonValue dom = v.ionValue();
            if (dom == null || ! klass.isInstance(dom))
            {
                exprs.add(expr);
            }
        }
        assert exprs.size() != 0;
        return exprs;
    }


    List<String> nonIntExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonInt.class);
    }

    List<String> nonTextExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonText.class);
    }

    List<String> nonContainerExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonContainer.class);
    }

    List<String> nonSequenceExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonSequence.class);
    }

    List<String> nonListExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonList.class);
    }

    List<String> nonStructExpressions()
        throws FusionException
    {
        return nonIonExpressions(IonStruct.class);
    }


    //========================================================================


    protected void assertEval(IonValue expected, String source)
        throws FusionException
    {
        FusionValue fv = myRuntime.eval(source);
        if (! fv.isIon())
        {
            Assert.fail("Result isn't ion: " + fv + "\nSource: " + source);
        }
        IonValue result = fv.ionValue();
        assertEquals(source, expected, result);
    }

    protected void assertEval(IonValue expected, IonValue expression)
        throws FusionException
    {
        String source = expression.toString();
        assertEval(expected, source);
    }

    protected void assertEval(String expectedIon, String expressionIon)
        throws FusionException
    {
        IonValue expected = mySystem.singleValue(expectedIon);
        assertEval(expected, expressionIon);
    }

    protected void assertUndef(String expressionIon)
        throws FusionException
    {
        FusionValue fv = myRuntime.eval(expressionIon);
        if (fv != FusionValue.UNDEF)
        {
            Assert.fail("Result isn't undef: " + fv + "\nSource: " + expressionIon);
        }
    }

    protected void assertEval(boolean expectedBool, String expressionIon)
        throws FusionException
    {
        IonValue expected = mySystem.newBool(expectedBool);
        assertEval(expected, expressionIon);
    }

    protected void assertEval(int expectedInt, String expressionIon)
        throws FusionException
    {
        IonValue expected = mySystem.newInt(expectedInt);
        assertEval(expected, expressionIon);
    }

    protected void assertEval(BigInteger expectedInt, String expressionIon)
        throws FusionException
    {
        FusionValue fv = myRuntime.eval(expressionIon);
        IonValue observed = fv.ionValue();
        if (observed instanceof IonInt)
        {
            IonInt iObsExp = (IonInt)observed;
            BigInteger obsExp = iObsExp.bigIntegerValue();
            if (obsExp.compareTo(expectedInt) == 0)
            {
                return;
            }
            Assert.fail("Discrepency: Observed "+obsExp.toString()+", expected "+expectedInt.toString());
        }
        Assert.fail("Invalid type.");
    }

    protected void assertBigInt(int expectedInt, String expressionIon)
        throws FusionException
    {
        BigInteger bExpInt = BigInteger.valueOf(expectedInt);
        assertEval(bExpInt, expressionIon);
    }

    protected void assertString(String expectedString, String expressionIon)
        throws FusionException
    {
        FusionValue fv = myRuntime.eval(expressionIon);
        IonValue observed = fv.ionValue();
        IonValue iv = fv.ionValue();
        if (iv instanceof IonString)
        {
            IonString is = (IonString)iv;
            String result = is.stringValue();
            assertEquals(expressionIon, expectedString, result);
            return;
        }
        Assert.fail("Input arg is of invalid type.");
    }

    protected void assertSelfEval(String expressionIon)
        throws FusionException
    {
        assertEval(expressionIon, expressionIon);
    }

    protected FusionValue eval(String expressionIon)
        throws FusionException
    {
        FusionValue result = myRuntime.eval(expressionIon);
        return result;
    }

    protected IonValue evalToIon(String expressionIon)
        throws FusionException
    {
        FusionValue result = eval(expressionIon);
        assertTrue("Result isn't Ion", result.isIon());
        return result.ionValue();
    }

    protected Procedure evalToProcedure(String expressionIon)
        throws FusionException
    {
        FusionValue result = eval(expressionIon);
        return (Procedure) result;
    }


    //========================================================================

    <T extends FusionException> T expectFailure(Class<T> klass, String expr)
            throws Exception
    {
        try
        {
            eval(expr);
            Assert.fail("Expected exception from " + expr);
            return null; // Dummy for compiler
        }
        catch (Exception e)
        {
            if (klass.isInstance(e))
            {
                return klass.cast(e);
            }
            throw e;
        }
    }

    void expectFusionException(String expr)
        throws Exception
    {
        expectFailure(FusionException.class, expr);
    }

    void expectSyntaxFailure(String expr)
        throws Exception
    {
        expectFailure(SyntaxFailure.class, expr);
    }


    void expectContractFailure(String expr)
        throws Exception
    {
        expectFailure(ContractFailure.class, expr);
    }


    void expectArityFailure(String expr)
        throws Exception
    {
        expectFailure(ArityFailure.class, expr);
    }

    void expectArgTypeFailure(String expr, int badArgNum)
        throws Exception
    {
        ArgTypeFailure e = expectFailure(ArgTypeFailure.class, expr);
        assertEquals("argument #", badArgNum, e.getArgNum());
    }

}
