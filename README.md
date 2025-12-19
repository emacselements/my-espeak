# my-espeak

Text-to-speech integration for Emacs using eSpeak and Edge TTS (neural voices).

Demo: https://youtube.com/shorts/M2eQxH-Lksk?feature=share

## Features

- **eSpeak Integration** - Fast, lightweight text-to-speech (F9)
- **Edge TTS** - High-quality neural voices from Microsoft (F10)
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
- `edge-tts` - Neural TTS with natural voices. Install via `pip install --user edge-tts`.
- Audio player: `paplay`, `ffplay`, `mplayer`, `vlc`, or `aplay` (one of these is required)

## Usage

### Basic Commands

- `F9` - Toggle eSpeak reading (fast, lightweight)
- `F10` - Toggle Fancy reading (Edge TTS neural voices)

Both commands read from cursor position or selected text in any buffer, including PDFs.

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
- `C-c o` - Select region with mouse, OCR it, read with Edge TTS

These commands let you visually select a region (including single columns in multi-column layouts), then OCR and read that specific area.

## Configuration

### TTS Options

- `F9` (default): eSpeak — fast, offline, lightweight.
- `F10` (fancy): Edge TTS — neural voices from Microsoft; requires internet connection.

### Example: Setup Edge TTS

Install Edge TTS:

```bash
pip install --user edge-tts

# Test it
edge-tts -t "Hello world" --write-media /tmp/test.mp3 && paplay /tmp/test.mp3
```

## How It Works

1. **Text Extraction** - Gets text from cursor/selection or entire PDF page
2. **Text Cleaning** - Joins hyphenated words, removes ligatures and footnotes
3. **Speech Synthesis** - Sends to eSpeak or Edge TTS
4. **Playback** - Reads aloud with progress tracking

## Files

- `my-espeak.el` - Main TTS module (eSpeak and Edge TTS)
- `pdf-ocr-espeak.el` - Optional OCR-based PDF reading
- `older/` - Previous versions and experimental scripts

## License

MIT
