// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.symbolVariant;
import static com.amazon.ion.util.IonTextUtils.SymbolVariant.IDENTIFIER;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * A unique identitier for modules available to the Fusion runtime system.
 * This plays the same role as Racket's "resolved module path".
 */
class ModuleIdentity
    implements Comparable<ModuleIdentity>
{
    static final String LOCAL_NAME_EXPECTATION =
        "Expected an Ion identifier";

    static final String BUILTIN_NAME_EXPECTATION =
        "Expected `#%` followed by an Ion identifier";

    private static final Map<String,ModuleIdentity> ourInternedIdentities =
        new HashMap<String,ModuleIdentity>();

    static boolean isValidLocalName(String name)
    {
        return name != null
            && symbolVariant(name) == IDENTIFIER;
    }

    static void validateLocalName(SyntaxSymbol name)
        throws FusionException
    {
        String text = name.stringValue();
        if (! isValidLocalName(text))
        {
            throw new SyntaxFailure("local module name",
                                    LOCAL_NAME_EXPECTATION, name);
        }
    }

    static boolean isValidBuiltinName(String name)
    {
        return name != null
            && name.startsWith("#%")
            && symbolVariant(name.substring(2)) == IDENTIFIER;
    }

    private static ModuleIdentity doIntern(String name)
    {
        ModuleIdentity interned = ourInternedIdentities.get(name);
        if (interned != null) return interned;

        ModuleIdentity id = new ModuleIdentity(name);
        ourInternedIdentities.put(name, id);
        return id;
    }


    /**
     * @param name must be a valid local module name.
     * @return not null.
     *
     * @see #isValidLocalName(String)
     */
    static ModuleIdentity internLocalName(String name)
    {
        assert isValidLocalName(name);
        return doIntern(name);
    }

    /**
     * @param name must be a valid builtin module name.
     * @return not null.
     *
     * @see #isValidBuiltinName(String)
     */
    static ModuleIdentity internBuiltinName(String name)
    {
        assert isValidBuiltinName(name);
        return doIntern(name);
    }

    /**
     *
     * @param name must be the result of {@link #internString()}.
     * @return not null.
     */
    static ModuleIdentity reIntern(String name)
    {
        ModuleIdentity interned = ourInternedIdentities.get(name);
        assert interned != null;
        return interned;
    }


    static ModuleIdentity internFromClasspath(String modulePath,
                                              final String resource)
    {
        assert modulePath.startsWith("/");
        assert resource.startsWith("/");

        ModuleIdentity interned = ourInternedIdentities.get(modulePath);
        if (interned != null) return interned;

        ModuleIdentity id = new ModuleIdentity(modulePath)
        {
            @Override
            public String identify()
            {
                return internString() + " (at classpath:" + resource + ")";
            }

            @Override
            InputStream open()
                throws IOException
            {
                return getClass().getResourceAsStream(resource);
            }
        };

        ourInternedIdentities.put(modulePath, id);
        return id;
    }


    static ModuleIdentity internFromFile(String modulePath, final File file)
    {
        assert modulePath.startsWith("/");
        assert file.isAbsolute();

        ModuleIdentity interned = ourInternedIdentities.get(modulePath);
        if (interned != null) return interned;

        ModuleIdentity id = new ModuleIdentity(modulePath)
        {
            @Override
            public String identify()
            {
                return internString() + " (at file:" + file + ")";
            }

            @Override
            InputStream open()
                throws IOException
            {
                return new FileInputStream(file);
            }

            @Override
            String parentDirectory()
            {
                return file.getParentFile().getAbsolutePath();
            }
        };

        ourInternedIdentities.put(modulePath, id);
        return id;
    }

    private final String myName;

    private ModuleIdentity(String name)
    {
        myName = name;
    }


    InputStream open()
        throws IOException
    {
        throw new UnsupportedOperationException();
    }

    String parentDirectory()
    {
        return null;
    }

    String identify()
    {
        return myName;
    }


    @Override
    public String toString()
    {
        return identify();
    }

    public String internString()
    {
        return myName;
    }

    public String baseName()
    {
        int slashIndex = myName.lastIndexOf('/');
        if (slashIndex == -1) return myName;
        return myName.substring(slashIndex + 1);
    }


    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((myName == null) ? 0 : myName.hashCode());
        return result;
    }


    @Override
    public boolean equals(Object obj)
    {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ModuleIdentity other = (ModuleIdentity) obj;
        if (myName == null)
        {
            if (other.myName != null) return false;
        }
        else if ( !myName.equals(other.myName)) return false;
        return true;
    }


    @Override
    public int compareTo(ModuleIdentity that)
    {
        return this.myName.compareTo(that.myName);
    }
}
