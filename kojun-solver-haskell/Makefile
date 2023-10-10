# Nome do executável
EXECUTABLE = main

# Diretório para os arquivos intermediários
OUTPUT_DIR = ./output

# Caminhos dos arquivos
SOURCE_FILES = main.hs
MODULE_FILES = utils/matrix.hs

# Comando para compilar o código
GHCCOMPILE = ghc -o $(EXECUTABLE) $(SOURCE_FILES) $(MODULE_FILES) -outputdir=$(OUTPUT_DIR) -hidir=$(OUTPUT_DIR) -odir=$(OUTPUT_DIR)

all:
	@echo "> Compiling..."
	$(GHCCOMPILE)
	@echo "> Compilation finished."

run: all
	./$(EXECUTABLE)

clean:
	@echo "> Cleaning output directory..."
	rm -rf $(OUTPUT_DIR)
	rm -f $(EXECUTABLE)
	@echo "> Cleaned output directory."

.PHONY: all run clean
