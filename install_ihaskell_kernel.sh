#!/bin/bash
# Script to install IHaskell kernel with proper package exposure

set -e

cd /opt/cabal-project

# Install the kernel
cabal exec -- ihaskell install --prefix=/usr/local

# Get the kernel.json path
KERNEL_JSON="/usr/local/share/jupyter/kernels/haskell/kernel.json"

if [ -f "$KERNEL_JSON" ]; then
    echo "Configuring kernel to expose hidden packages..."
    
    # Backup original
    cp "$KERNEL_JSON" "${KERNEL_JSON}.backup"
    
    # Create a .ghc.environment file in the home directory
    # This will be automatically picked up by GHC
    GHC_ENV_FILE="$HOME/.ghc.environment.x86_64-linux-$(ghc --numeric-version)"
    
    echo "Creating GHC environment file: $GHC_ENV_FILE"
    
    # Get package databases
    GLOBAL_DB=$(ghc --print-global-package-db)
    
    # Start the environment file
    cat > "$GHC_ENV_FILE" << EOF
clear-package-db
global-package-db
EOF
    
    # Add user package database if it exists
    if cabal exec -- ghc-pkg list --user > /dev/null 2>&1; then
        USER_DB=$(cabal exec -- ghc-pkg list --user | grep ':' | head -1 | sed 's/://')
        if [ -d "$USER_DB" ]; then
            echo "package-db $USER_DB" >> "$GHC_ENV_FILE"
        fi
    fi
    
    # Get full package IDs for all packages and add them
    echo "Adding packages to environment file..."
    
    for pkg in template-haskell unix directory process filepath containers bytestring array vector text time random transformers mtl; do
        PKG_ID=$(cabal exec -- ghc-pkg field $pkg id --simple-output 2>/dev/null | head -1)
        if [ ! -z "$PKG_ID" ]; then
            echo "package-id $PKG_ID" >> "$GHC_ENV_FILE"
            echo "  Added: $pkg ($PKG_ID)"
        fi
    done
    
    # Add custom packages
    for pkg in dataframe ihaskell-aeson ihaskell-blaze ihaskell-gnuplot ihaskell-graphviz ihaskell-hatex ihaskell-juicypixels ihaskell-widgets dataframe-hasktorch ihaskell-dataframe hasktorch; do
        PKG_ID=$(cabal exec -- ghc-pkg field $pkg id --simple-output 2>/dev/null | head -1)
        if [ ! -z "$PKG_ID" ]; then
            echo "package-id $PKG_ID" >> "$GHC_ENV_FILE"
            echo "  Added: $pkg ($PKG_ID)"
        fi
    done
    
    echo ""
    echo "✓ GHC environment file created at: $GHC_ENV_FILE"
    echo "✓ Kernel installation completed successfully!"
    echo "✓ Kernel location: $KERNEL_JSON"
    
    # Show the environment file for debugging
    echo ""
    echo "Environment file contents:"
    cat "$GHC_ENV_FILE"
else
    echo "Error: Kernel JSON file not found at $KERNEL_JSON"
    exit 1
fi
