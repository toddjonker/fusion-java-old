// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;

import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import org.junit.After;


public class CoreTestCase
{
    private IonSystem mySystem = IonSystemBuilder.standard().build();
    private Environment myEnvironment = new CoreEnvironment(mySystem);
    private Evaluator myEvaluator = new Evaluator();

    @After
    public void tearDown()
    {
        mySystem = null;
        myEnvironment = null;
        myEvaluator = null;
    }

    //========================================================================

    protected void assertEval(String expectedIon, String expressionIon)
    {
        IonValue expression = mySystem.singleValue(expressionIon);
        IonValue expected   = mySystem.singleValue(expectedIon);
        
        IonValue result = myEvaluator.evalToIon(myEnvironment, expression);
        
        assertEquals(expected, result);
    }
    
    protected void assertSelfEval(String expressionIon)
    {
        assertEval(expressionIon, expressionIon);
    }
}
