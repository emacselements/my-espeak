# my-espeak

Text-to-speech integration for Emacs using eSpeak and Piper (neural TTS).

Demo: https://youtube.com/shorts/M2eQxH-Lksk?feature=share

## Features

- **eSpeak Integration** - Fast, lightweight text-to-speech (F9)
- **Piper TTS** - High-quality neural voices, offline and fast (F10)
- **PDF Support** - Read PDF text aloud from cursor or selection
- **Smart Text Processing** - Handles hyphenated words, ligatures, and footnotes
- **Toggle Controls** - Easy start/stop with keyboard shortcuts

## Installation

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/my-espeak.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/my-espeak")
   (require 'my-espeak)
   ```

### Dependencies

**Required:**
- `espeak` - For basic text-to-speech

**Optional / Fancy (recommended):**
- `piper-tts` - Modern neural TTS with natural voices. Install via `pip install --user piper-tts`.
- Voice model - Download automatically on first use or manually from [Piper voices](https://huggingface.co/rhasspy/piper-voices/tree/main/en/en_US)
- Audio player: `paplay`, `ffplay`, `mplayer`, `vlc`, or `aplay` (one of these is required)

## Usage

### Basic Commands

- `F9` - Toggle eSpeak reading (fast, lightweight)
- `F10` - Toggle Fancy reading (Piper neural TTS) (natural-sounding, offline)

Both commands read from cursor position or selected text in any buffer, including PDFs. The fancy option uses Piper for modern neural voices with natural pronunciation and no network dependency.

## Optional: PDF OCR-Based Reading

For multi-column PDFs where standard text extraction doesn't respect column boundaries, use the optional `pdf-ocr-espeak.el` module.

### OCR Module Requirements

The OCR functionality depends on [screenshot-ocr](https://github.com/emacselements/screenshot-ocr):

**System dependencies:**
- `maim` - Screenshot tool
- `tesseract` - OCR engine

**Installation:**

1. Install system dependencies:
   ```bash
   # Arch Linux
   sudo pacman -S maim tesseract

   # Ubuntu/Debian
   sudo apt install maim tesseract-ocr
   ```

2. Clone screenshot-ocr:
   ```bash
   git clone https://github.com/emacselements/screenshot-ocr.git
   ```

3. Load the OCR module in your config:
   ```elisp
   (require 'pdf-ocr-espeak)
   ```

### OCR Commands (in pdf-view-mode)

- `O` - Select region with mouse, OCR it, read with eSpeak
- `C-c o` - Select region with mouse, OCR it, read with Piper TTS

These commands let you visually select a region (including single columns in multi-column layouts), then OCR and read that specific area.

## Configuration

### TTS Options

- `F9` (default): eSpeak — fast, offline, lightweight.
- `F10` (fancy): Piper — modern neural TTS with natural-sounding voices; completely offline.

For other options, consider `festival` or `spd-say` for alternatives.

### Example: Setup Piper TTS

Install Piper and download a voice model:

```bash
pip install --user piper-tts

# Download a natural voice model
mkdir -p ~/.local/share/piper/voices
cd ~/.local/share/piper/voices
wget https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/lessac/medium/en_US-lessac-medium.onnx
wget https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/lessac/medium/en_US-lessac-medium.onnx.json

# Test it
echo "Hello world" | python3 -m piper -m en_US-lessac-medium.onnx -f /tmp/test.wav && paplay /tmp/test.wav
```

### Notes

- Piper is completely offline and uses modern neural networks for natural-sounding speech.
- For long texts that exceed online TTS service limits, the code may split or fall back to `espeak`.

## How It Works

1. **Text Extraction** - Gets text from cursor/selection or entire PDF page
2. **Text Cleaning** - Joins hyphenated words, removes ligatures and footnotes
3. **Speech Synthesis** - Sends to eSpeak, Piper, or other configured TTS engine
4. **Playback** - Reads aloud with progress tracking

## Files

- `my-espeak.el` - Main TTS module (eSpeak and Piper)
- `pdf-ocr-espeak.el` - Optional OCR-based PDF reading
- `older/` - Previous versions and experimental scripts

## License

MIT
