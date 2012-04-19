// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import org.junit.After;
import org.junit.Assert;


public class CoreTestCase
{
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

    protected void assertEval(IonValue expected, IonValue expression)
    {
        try
        {
            IonValue result = myEvaluator.evalToIon(myEnvironment, expression);
            assertEquals(expression.toString(), expected, result);
        }
        catch (FusionException e)
        {
            throw new AssertionError(e);
        }

    }

    protected void assertEval(String expectedIon, String expressionIon)
    {
        IonValue expected   = mySystem.singleValue(expectedIon);
        IonValue expression = mySystem.singleValue(expressionIon);

        assertEval(expected, expression);
    }

    protected void assertEval(int expectedInt, String expressionIon)
    {
        IonValue expected   = mySystem.newInt(expectedInt);
        IonValue expression = mySystem.singleValue(expressionIon);

        assertEval(expected, expression);
    }

    protected void assertSelfEval(String expressionIon)
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

    protected void evalToFunction(String expressionIon)
        throws FusionException
    {
        FusionValue result = eval(expressionIon);
        Assert.assertTrue(result instanceof FunctionValue);
    }
}
