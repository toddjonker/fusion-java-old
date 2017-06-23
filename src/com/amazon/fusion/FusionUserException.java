package com.amazon.fusion;

import java.io.IOException;

class FusionUserException
    extends FusionException
{
    private final Object exceptionValue;

    public FusionUserException(String message, Object exceptionValue)
    {
        super(message);
        this.exceptionValue = exceptionValue;
    }

    public FusionUserException(String message,
                               Throwable cause,
                               Object exceptionValue)
    {
        super(message, cause);
        this.exceptionValue = exceptionValue;
    }

    public FusionUserException(Throwable cause, Object exceptionValue)
    {
        super(cause.getMessage(), cause);
        this.exceptionValue = exceptionValue;
    }

    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        FusionIo.write(eval, out, exceptionValue);
        super.displayMessage(eval, out);
    }

    /**
     * For use in with_handlers and CallWithHandlerProc.
     * @return the exception value held within the FusionException
     */
    public final Object getExceptionValue()
    {
        return exceptionValue;
    }
}
