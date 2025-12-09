# my-espeak

Text-to-speech integration for Emacs using eSpeak and Google TTS (gTTS).

## Features

- **eSpeak Integration** - Fast, lightweight text-to-speech (F9)
- **Google TTS (gTTS)** - Higher quality cloud voices (F10)
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
- `gTTS` (Google Text-to-Speech CLI) - High-quality cloud voices. Install via `pip install gTTS`.
- Audio player: `paplay`, `ffplay`, `mplayer`, `vlc`, or `aplay` (one of these is required for audio playback)

## Usage

### Basic Commands

- `F9` - Toggle eSpeak reading (fast, lightweight)
- `F10` - Toggle Fancy reading (Google TTS/gTTS) (higher quality)

Both commands read from cursor position or selected text in any buffer, including PDFs. The fancy option uses `gtts-cli` by default; if `gtts-cli` is not available the code will fall back to `espeak`.

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
- `C-c o` - Select region with mouse, OCR it, read with Google TTS (gTTS)

These commands let you visually select a region (including single columns in multi-column layouts), then OCR and read that specific area.

## Configuration

### TTS Options

- `F9` (default): eSpeak — fast, offline, lightweight.
- `F10` (fancy): Google TTS (`gtts-cli`) — higher-quality cloud voices; requires internet.

If you prefer offline neural or male voices, consider `festival` (install via package manager) or `spd-say`/`flite` for alternatives.

### Example: Ensure `gtts-cli` is available

Install the Python package and test from a terminal:

```bash
pip install --user gTTS
gtts-cli "Hello world" --output /tmp/test_gtts.mp3
paplay /tmp/test_gtts.mp3
```

### Notes

- `gtts-cli` sends requests to Google's TTS service and requires network connectivity.
- For long texts that exceed online TTS service limits, the code may split or fall back to `espeak`.

## How It Works

1. **Text Extraction** - Gets text from cursor/selection or entire PDF page
2. **Text Cleaning** - Joins hyphenated words, removes ligatures and footnotes
3. **Speech Synthesis** - Sends to eSpeak, gTTS, or other configured TTS engine
4. **Playback** - Reads aloud with progress tracking

## Author

Author: Raoul Comninos

## License

MIT

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)
