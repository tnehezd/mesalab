import os
from tqdm import tqdm
import time
import logging
import sys

class TqdmLoggingHandler(logging.Handler):
    def emit(self, record):
        try:
            msg = self.format(record)
            tqdm.write(msg, file=sys.stderr)
        except Exception:
            self.handleError(record)

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s', handlers=[TqdmLoggingHandler()])

logging.info("Checking TQDM_ASCII environment variable...")

# Set the environment variable
os.environ['TQDM_ASCII'] = '1'
logging.info(f"TQDM_ASCII set to: {os.environ.get('TQDM_ASCII')}")

logging.info("Starting tqdm with TQDM_ASCII='1'")
for i in tqdm(range(50), desc="ASCII Progress"):
    time.sleep(0.05)
logging.info("Finished ASCII progress.")

# Try removing it and see if it changes (optional, for testing)
del os.environ['TQDM_ASCII']
logging.info(f"TQDM_ASCII removed. Value now: {os.environ.get('TQDM_ASCII')}")
logging.info("Starting tqdm without TQDM_ASCII (should show Unicode or ??? depending on locale)")
for i in tqdm(range(50), desc="Unicode Progress"):
    time.sleep(0.05)
logging.info("Finished Unicode progress.")