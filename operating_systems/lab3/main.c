#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ftw.h>
#include <getopt.h>

#define MAXBUFLEN 1000000
#define MAXFILENAMELEN 1024

char* g_inputDir = NULL;
char* g_outputDir = NULL;

void reverseString(char* line, int len) {
    int start = 0;
    int end = len - 1;
    char temp;
    // Simple string reversing algorithm
    while (start < end) {
        temp = line[start];
        line[start] = line[end];
        line[end] = temp; start++;
        end--;
    }
}

// Return pointer to end of this line (in other words to start of next line)
char* getLine(char* source, int* lineLength) {
    *lineLength = 0;
    while (*source != '\0' && *source != '\n') {
        source++;
        (*lineLength)++;
    }

    // Include new-line in resulting string
    if (*source == '\n') {
        (*lineLength)++;
    }
    return source;
}

void reverseLinesInFile(const char* inputFileName, const char* outFileName) {
    char source[MAXBUFLEN + 1];
    FILE *fp = fopen(inputFileName, "r");
    FILE *out_fp = fopen(outFileName, "w");

    if (fp != NULL && out_fp != NULL) {
        size_t newLen = fread(source, sizeof(char), MAXBUFLEN, fp);
        if (ferror(fp) != 0) {
            fputs("Error reading file", stderr);
        } else {
            source[newLen++] = '\0'; /* Ensure null termination. */
        }

        fclose(fp);

        char* currentLine = source;
        int lineLength;
        while (*currentLine != '\0') {
            char* nextLine = getLine(currentLine, &lineLength);
            // This ensures that \n char isn't reversed as reversing \n can lead to damaging file structure
            // eg. Hello\n -> \nolleH (which is wrong)
            // this line ensures that Hello\n -> olleH\n
            if (lineLength > 0) {
                reverseString(currentLine, lineLength - 1);
                fwrite(currentLine, sizeof(char), lineLength, out_fp);
            }
            // Edge case, depending on how we exited from getLine func
            currentLine = *nextLine == '\n' ? nextLine + 1 : nextLine;
        }

        fclose(out_fp);
    } else {
        fputs("Error while opening files", stderr);
    }
}

//// TODO: This func works only for small files (total char count < MAXBUFLEN)
//// rewrite it in later implementations so it load smaller buffer and operate on lines directly
int processFile(const char *path, const struct stat *sb, int typeflag, struct FTW *ftwbuf) {
    if (typeflag == FTW_F) {
        // Just get all .txt files from and apply reverse func on them
        // put result into fresh new results/*_reversed.txt file
        const char *ext = strrchr(path, '.');
        if (ext != NULL && strcmp(ext, ".txt") == 0) {
            // Remove leadning path from fileName eg. from 'foo/bar/asdf' to 'asdf'
            const char *filename = strrchr(path, '/');
            if (filename == NULL) {
                filename = path;
            } else {
                filename++;
            }

            char outFileName[MAXFILENAMELEN];
            strcpy(outFileName, g_outputDir);
            strcat(outFileName, "/");
            strcat(outFileName, filename);
            *(strrchr(outFileName, '.')) = '\0';
            strcat(outFileName, "_reversed.txt");
            reverseLinesInFile(path, outFileName);
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    int opt;

    // "i:o:" - specify which flags are keywords accepted
    while ((opt = getopt(argc, argv, "i:o:")) != -1) {
        switch (opt) {
            case 'i':
                // opetarg stores next value after current flag (-i <inputDir>) -> then optarg is char* to <inputDir>
                g_inputDir = optarg;
                break;
            case 'o':
                g_outputDir = optarg;
                break;
            default:
                fprintf(stderr, "Usage: %s -i <inputDir> -o <outputDir>\n", argv[0]);
                return 1;
        }
    }

    if (!g_inputDir || !g_outputDir) {
        fprintf(stderr, "Missing required arguments.\n");
        fprintf(stderr, "Usage: %s -i <inputDir> -o <outputDir>\n", argv[0]);
        return 1;
    }

    mkdir(g_outputDir, 0777);

    nftw(g_inputDir, processFile, 10, FTW_PHYS);
    return 0;
}
