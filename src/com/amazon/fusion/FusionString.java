// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionSymbol.makeSymbol;
import static com.amazon.fusion.FusionText.checkRequiredTextArg;
import static com.amazon.fusion.FusionUtils.safeEquals;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionText.BaseText;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.Arrays;


/**
 * Utilities for working with Fusion {@code string} values.
 *
 * @see FusionValue
 */
public final class FusionString
{
    private FusionString() {}

    /** The string {@code "UTF-8"}. */
    static final String UTF8_CHARSET_NAME = "UTF-8";

    static final Charset UTF8_CHARSET = Charset.forName(UTF8_CHARSET_NAME);


    //========================================================================
    // Representation Classes


    abstract static class BaseString
        extends BaseText
    {
        private BaseString() {}

        @Override
        BaseString annotate(Evaluator eval, String[] annotations)
        {
            return FusionString.annotate(this, annotations);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseString)
            {
                String r = ((BaseString) right).stringValue();
                if (r != null)
                {
                    String l = this.stringValue(); // not null
                    if (l.equals(r))
                    {
                        return trueBool(eval);
                    }
                }
            }

            return falseBool(eval);
        }

        @Override
        SyntaxValue toStrippedSyntaxMaybe(Evaluator eval)
        {
            return SyntaxString.make(eval, /*location*/ null, this);
        }

        @Override
        public boolean equals(Object o)
        {
            if (this == o) return true;
            if (o instanceof BaseString)
            {
                BaseString that = (BaseString) o;
                return (safeEquals(this.stringValue(), that.stringValue())
                        && Arrays.equals(this.annotationsAsJavaStrings(),
                                         that.annotationsAsJavaStrings()));
            }
            return false;
        }
    }


    private static class NullString
        extends BaseString
    {
        private NullString() {}

        @Override
        String stringValue()
        {
            return null;
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseString
                         && ((BaseString) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullString();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.STRING);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.string");
        }
    }


    private static class ActualString
        extends BaseString
    {
        private final String myContent;

        private ActualString(String content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        String stringValue()
        {
            return myContent;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newString(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeString(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            IonTextUtils.printString(out, myContent);
        }

        @Override
        void display(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append(myContent);
        }
    }


    private static class AnnotatedString
        extends BaseString
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseString  myValue;

        private AnnotatedString(String[] annotations, BaseString value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        BaseString annotate(Evaluator eval, String[] annotations)
        {
            return FusionString.annotate(myValue, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        String stringValue()
        {
            return myValue.stringValue();
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.tightEquals(eval, right);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = myValue.copyToIonValue(factory,
                                                 throwOnConversionFailure);
            iv.setTypeAnnotations(myAnnotations);
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(myAnnotations);
            myValue.ionize(eval, out);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            myValue.write(eval, out);
        }
    }


    //========================================================================
    // Constructors


    private static final BaseString NULL_STRING = new NullString();


    /**
     * @param value may be null to make {@code null.string}.
     *
     * @return not null.
     */
    static BaseString makeString(Evaluator eval, String value)
    {
        return (value == null ? NULL_STRING : new ActualString(value));
    }


    private static BaseString annotate(BaseString unannotated,
                                       String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedString);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedString(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.string}.
     *
     * @return not null.
     */
    static BaseString makeString(Evaluator eval,
                                 String[]  annotations,
                                 String    value)
    {
        BaseString base = makeString(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionString must be a Fusion string.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseString unsafeStringAnnotate(Evaluator eval,
                                           Object fusionString,
                                           String[] annotations)
    {
        BaseString base = (BaseString) fusionString;
        return base.annotate(eval, annotations);
    }


    //========================================================================
    // Predicates


    public static boolean isString(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseString);
    }

    static boolean isString(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseString);
    }


    //========================================================================
    // Conversions


    /**
     * @param fusionString must be a Fusion string.
     *
     * @return null if given {@code null.string}.
     */
    static String unsafeStringToJavaString(Evaluator eval, Object fusionString)
        throws FusionException
    {
        return ((BaseString) fusionString).stringValue();
    }


    /**
     * Converts a Fusion string to a {@link String}.
     *
     * @return null if the value isn't a Fusion string.
     */
    static String stringToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isString(eval, value))
        {
            return unsafeStringToJavaString(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static String checkStringArg(Evaluator eval,
                                 Procedure who,
                                 String    expectation,
                                 int       argNum,
                                 Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseString)
        {
            return ((BaseString) arg).stringValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return may be null
     */
    static String checkNullableStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable string";
        return checkStringArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static String checkRequiredStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    /**
     * @return not null or empty
     */
    static String checkNonEmptyStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-empty string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null || result.isEmpty())
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Procedures


    static final class IsStringProc
        extends Procedure1
    {
        IsStringProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `string`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isString(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class SizeUtf8
        extends Procedure1
    {
        SizeUtf8()
        {
            super("Docs in Fusion",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            String s = checkNullableStringArg(eval, this, 0, arg);

            if (s == null || s.isEmpty()) return FusionNumber.ZERO_INT;

            CharsetEncoder encoder = UTF8_CHARSET.newEncoder();
            try
            {
                ByteBuffer buffer = encoder.encode(CharBuffer.wrap(s));
                int size = buffer.limit();
                return makeInt(eval, size);
            }
            catch (CharacterCodingException e)
            {
                throw argFailure("valid Unicode string", 0, arg);
            }
        }
    }


    static final class AppendProc
        extends Procedure
    {
        AppendProc()
        {
            //    "                                                                               |
            super("Concatenates the `text` values (strings or symbols), returning a string.  If no\n" +
                  "arguments are supplied, the result is `\"\"`.",
                  "text", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            StringBuilder resultBuilder = new StringBuilder();

            for (int i = 0; i < args.length; i++)
            {
                String v = checkRequiredTextArg(eval, this, i, args);
                resultBuilder.append(v);
            }

            return makeString(eval, resultBuilder.toString());
        }
    }



    static final class ToLowerProc
        extends Procedure
    {
        ToLowerProc()
        {
            super("Converts all the characters in a `string` to lower-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkRequiredStringArg(eval, this, 0, args);
            return makeString(eval, input.toLowerCase());
        }
    }



    static final class ToUpperProc
        extends Procedure
    {
        ToUpperProc()
        {
            super("Converts all the characters in a `string` to upper-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkRequiredStringArg(eval, this, 0, args);
            return makeString(eval, input.toUpperCase());
        }
    }



    static final class ToSymbolProc
        extends Procedure
    {
        ToSymbolProc()
        {
            //    "                                                                               |
            super("Converts a `string` to a symbol with the same text.  Returns `null.symbol` when\n"
                + "given `null.string`.  Raises an exception when given an empty string.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableStringArg(eval, this, 0, args);

            if (input != null && input.isEmpty())
            {
                throw argFailure("non-empty string", 0, args);
            }

            return makeSymbol(eval, input);
        }
    }
}
