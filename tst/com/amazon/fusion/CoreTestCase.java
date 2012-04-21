// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import com.amazon.ion.IonContainer;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.junit.After;
import org.junit.Assert;


public class CoreTestCase
{
    static final String NONTERMINATING_EXPRESSION =
        "((func (x) (x x)) (func (x) (x x)))";

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
        "(func () 1)",
    };


    //========================================================================


    private IonSystem mySystem = IonSystemBuilder.standard().build();
    private Environment myEnvironment = new CoreEnvironment(mySystem);
    private Evaluator myEvaluator = new Evaluator(mySystem);

    @After
    public void tearDown()
    {
        mySystem = null;
        myEnvironment = null;
        myEvaluator = null;
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
            IonValue dom = v.getDom();
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


    protected void assertEval(IonValue expected, IonValue expression)
        throws FusionException
    {
        IonValue result = myEvaluator.evalToIon(myEnvironment, expression);
        assertEquals(expression.toString(), expected, result);
    }

    protected void assertEval(String expectedIon, String expressionIon)
        throws FusionException
    {
        IonValue expected   = mySystem.singleValue(expectedIon);
        IonValue expression = mySystem.singleValue(expressionIon);

        assertEval(expected, expression);
    }

    protected void assertEval(boolean expectedBool, String expressionIon)
        throws FusionException
    {
        IonValue expected   = mySystem.newBool(expectedBool);
        IonValue expression = mySystem.singleValue(expressionIon);

        assertEval(expected, expression);
    }

    protected void assertEval(int expectedInt, String expressionIon)
        throws FusionException
    {
        IonValue expected   = mySystem.newInt(expectedInt);
        IonValue expression = mySystem.singleValue(expressionIon);

        assertEval(expected, expression);
    }

    protected void assertSelfEval(String expressionIon)
        throws FusionException
    {
        assertEval(expressionIon, expressionIon);
    }

    protected FusionValue eval(String expressionIon)
        throws FusionException
    {
        IonValue expression = mySystem.singleValue(expressionIon);
        FusionValue result = myEvaluator.eval(myEnvironment, expression);
        return result;
    }

    protected IonValue evalToIon(String expressionIon)
        throws FusionException
    {
        FusionValue result = eval(expressionIon);
        Assert.assertTrue(result instanceof DomValue);
        return result.getDom();
    }

    protected void evalToFunction(String expressionIon)
        throws FusionException
    {
        FusionValue result = eval(expressionIon);
        Assert.assertTrue(result instanceof FunctionValue);
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
