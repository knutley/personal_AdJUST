import os
import glob

# Delete all but the most recent checkpoint
checkpoint_pattern = "checkpoint_*.json"  # adjust pattern to match your files
checkpoints = sorted(glob.glob(checkpoint_pattern))

# Keep only the latest one
for old_checkpoint in checkpoints[:-1]:
    os.remove(old_checkpoint)
    print(f"Deleted {old_checkpoint}")
