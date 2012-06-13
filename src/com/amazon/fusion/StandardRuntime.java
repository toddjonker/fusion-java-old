// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;

/**
 *
 */
final class StandardRuntime
    implements FusionRuntime
{
    private final IonSystem mySystem;
    private final Evaluator myEvaluator;
    private final Environment myEnvironment;


    /**
     * WARNING: if the IonSystem is ever configurable, be sure to verify that
     * {@link FusionValue#ionValue(com.amazon.ion.ValueFactory)} works right!
     *
     * @see {@link FusionValueTest#testIonValue()}.
     */
    StandardRuntime()
    {
        mySystem = IonSystemBuilder.standard().build();
        myEvaluator = new Evaluator(mySystem);
        try
        {
            myEnvironment = myEvaluator.newBaseNamespace();
        }
        catch (FusionException e)
        {
            throw new RuntimeException("This shouldn't happen!", e);
        }
    }


    //========================================================================


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     */
    @Override
    public FusionValue eval(String source)
        throws ExitException, FusionException
    {
        IonReader i = mySystem.newReader(source);
        return eval(i);
    }


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException
     */
    @Override
    public FusionValue eval(IonReader source)
        throws ExitException, FusionException
    {
        FusionValue result = UNDEF;

        // TODO should work even if already positioned on first value

        while (source.next() != null)
        {
            IonValue sourceExpr = mySystem.newValue(source);
            result = myEvaluator.eval(myEnvironment, sourceExpr);
        }

        return result;
    }
}
