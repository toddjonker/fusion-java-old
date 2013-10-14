// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

/**
 * A unique identitier for modules available to the Fusion runtime system.
 * This plays the same role as Racket's "resolved module path".
 */
class ModuleIdentity
    implements Comparable<ModuleIdentity>
{
    static final String TOP_LEVEL_MODULE_PREFIX =
        "/fusion/private/toplevel/";

    static final String LOCAL_NAME_EXPECTATION =
        "Expected an Ion identifier";

    static final String BUILTIN_NAME_EXPECTATION =
        "Expected an absolute module path";

    /**
     * Access to this map must be synchronized on it!
     */
    private static final Map<String,ModuleIdentity> ourInternedIdentities =
        new HashMap<>();

    /**
     * Counts the number of synthetic top-level module identities that have
     * been created.
     *
     * @see #forTopLevel()
     */
    private static final AtomicLong ourTopLevelCounter = new AtomicLong();


    private static Pattern NAME_PATTERN =
        Pattern.compile("[a-zA-Z][a-zA-Z0-9_]*");

    private static Pattern PATH_PATTERN =
        Pattern.compile("(/(" + NAME_PATTERN + "))+");

    static boolean isValidModuleName(String name)
    {
        return name != null && NAME_PATTERN.matcher(name).matches();
    }

    static boolean isValidAbsoluteModulePath(String path)
    {
        return path != null && PATH_PATTERN.matcher(path).matches();
    }

    static boolean isValidModulePath(String path)
    {
        return isValidModuleName(path) || isValidAbsoluteModulePath(path);
    }

    static boolean isValidLocalName(String name)
    {
        return isValidModuleName(name);
    }

    static void validateLocalName(SyntaxSymbol name)
        throws FusionException
    {
        String text = name.stringValue();
        if (! isValidLocalName(text))
        {
            throw new SyntaxException("local module name",
                                      LOCAL_NAME_EXPECTATION, name);
        }
    }


    private static ModuleIdentity doIntern(String name)
    {
        synchronized (ourInternedIdentities)
        {
            ModuleIdentity interned = ourInternedIdentities.get(name);
            if (interned != null) return interned;

            ModuleIdentity id = new ModuleIdentity(name);
            ourInternedIdentities.put(name, id);
            return id;
        }
    }


    private static String localPath(Namespace ns, String name)
    {
        assert isValidLocalName(name) : name;
        return ns.getModuleId().internString() + '/' + name;
    }


    /**
     * @param name must be a valid local module name.
     * @return not null.
     *
     * @see #isValidLocalName(String)
     */
    static ModuleIdentity internLocalName(Namespace ns, String name)
    {
        String path = localPath(ns, name);
        return doIntern(path);
    }

    /**
     * @param path must be an absolute module path.
     * @return not null.
     *
     * @see #isValidAbsoluteModulePath(String)
     */
    static ModuleIdentity internBuiltinName(String path)
    {
        assert isValidAbsoluteModulePath(path);
        return doIntern(path);
    }


    static ModuleIdentity locate(String absoluteModulePath)
    {
        synchronized (ourInternedIdentities)
        {
            return ourInternedIdentities.get(absoluteModulePath);
        }
    }

    // TODO merge with above?
    /**
     * @param baseModule must not be null.
     * @param relativePath must not be null.
     * @return null if the referenced module hasn't been interned.
     */
    static ModuleIdentity locateRelative(ModuleIdentity baseModule,
                                         String relativePath)
    {
        assert ! relativePath.startsWith("/");
        String basePath = baseModule.relativeBasePath();
        return locate(basePath + relativePath);
    }


    /**
     * Looks for an interned local module ID, but doesn't intern a new one.
     *
     * @return null if the local module hasn't been interned.
     */
    static ModuleIdentity locateLocal(Namespace ns, String name)
    {
        return locate(localPath(ns, name));
    }

    /**
     *
     * @param name must be the result of {@link #internString()}.
     * @return not null.
     */
    static ModuleIdentity reIntern(String name)
    {
        ModuleIdentity interned = locate(name);
        assert interned != null;
        return interned;
    }


    /**
     * Creates a <em>non-interned</em> identity for a top-level namespace.
     *
     * @return a fresh, non-interned identity.
     */
    static ModuleIdentity forTopLevel()
    {
        // TODO In a long-running service this could overflow.
        String path =
            TOP_LEVEL_MODULE_PREFIX + ourTopLevelCounter.incrementAndGet();

        ModuleIdentity id = new ModuleIdentity(path)
        {
            @Override
            String relativeBasePath()
            {
                return internString() + "/";
            }
        };

        return id;
    }

    static ModuleIdentity internFromClasspath(String modulePath,
                                              final String resource)
    {
        assert modulePath.startsWith("/");
        assert resource.startsWith("/");

        synchronized (ourInternedIdentities)
        {
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
    }


    static ModuleIdentity internFromFile(String modulePath, final File file)
    {
        assert modulePath.startsWith("/");
        assert file.isAbsolute();

        synchronized (ourInternedIdentities)
        {
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
    }

    /** Not null or empty */
    private final String myName;

    private ModuleIdentity(String name)
    {
        myName = name;
    }


    InputStream open()
        throws IOException
    {
        throw new UnsupportedOperationException("Cannot open " + myName);
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

    String relativeBasePath()
    {
        String path = myName;
        int slashIndex = path.lastIndexOf('/');
        assert slashIndex >= 0;
        if (slashIndex == 0)
        {
            path = "/";
        }
        else
        {
            path = path.substring(0, slashIndex + 1);
        }

        return path;
    }


    @Override
    public int hashCode()
    {
        final int prime = 31;
        return prime + myName.hashCode();
    }


    @Override
    public boolean equals(Object obj)
    {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ModuleIdentity other = (ModuleIdentity) obj;
        return myName.equals(other.myName);
    }


    @Override
    public int compareTo(ModuleIdentity that)
    {
        return this.myName.compareTo(that.myName);
    }
}
