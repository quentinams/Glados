# Variables
HC = ghc
SRC = src
TEMP = temp
TARGET = parse_char

# Options pour GHC
GHCFLAGS = -outputdir $(TEMP) -o $(TARGET)

# Règle par défaut
all: $(TARGET)

# Règle pour construire votre binaire
$(TARGET):
	$(HC) $(GHCFLAGS) $(SRC)/*.hs

# Règle pour nettoyer les fichiers intermédiaires et le binaire
clean:
	rm -rf $(TEMP) $(TARGET)

re:
	make clean
	make all

.PHONY: all clean
