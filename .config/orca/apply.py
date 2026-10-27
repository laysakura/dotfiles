#!/usr/bin/env python3
"""Apply portable Orca preferences to an existing, stopped local profile (1.4.242)."""
import hashlib
import json
import os
from pathlib import Path
import plistlib
import shutil
import sqlite3
import subprocess
import time


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')
    temporary.replace(path)


def main():
    home = Path.home()
    source = Path(__file__).resolve().parent
    with Path('/Applications/Orca.app/Contents/Info.plist').open('rb') as info:
        version = plistlib.load(info)['CFBundleShortVersionString']
    if version != '1.4.242':
        raise SystemExit(f'Unverified Orca version {version}; review the storage format first.')
    # An active app caches settings; never edit its database behind its back.
    running = subprocess.run(['pgrep', '-x', 'Orca'], capture_output=True)
    if running.returncode != 1:
        raise SystemExit('Quit Orca before applying settings (no processes may remain).')
    profile = home / 'Library/Application Support/Orca/profiles/local-default'
    database = profile / 'profile-state.db'
    if not database.is_file() or not database.with_suffix('.db.authority').is_file():
        raise SystemExit('Expected the initialized Orca SQLite local-default profile.')
    settings = json.loads((source / 'settings.json').read_text())
    bindings_patch = json.loads((source / 'keybindings.json').read_text())
    bindings_path = home / '.orca/keybindings.json'
    bindings = json.loads(bindings_path.read_text()) if bindings_path.exists() else {'version': 1}
    bindings.setdefault('platforms', {}).setdefault('darwin', {}).update(bindings_patch['platforms']['darwin'])
    rule_file = source / 'karabiner.json'
    rule = json.loads(rule_file.read_text())['rules'][0]
    karabiner_path = home / '.config/karabiner/karabiner.json'
    karabiner = json.loads(karabiner_path.read_text())
    selected = [p for p in karabiner['profiles'] if p.get('selected')]
    if len(selected) != 1:
        raise SystemExit('Expected exactly one selected Karabiner profile.')
    rules = selected[0].setdefault('complex_modifications', {}).setdefault('rules', [])
    rules[:] = [r for r in rules if r.get('description') != rule['description']]
    rules.insert(0, rule)

    backup = home / '.local/state/orca-dotfiles' / str(time.time_ns())
    backup.mkdir(parents=True, mode=0o700)
    for label, path in [('keybindings.json', bindings_path), ('karabiner.json', karabiner_path)]:
        if path.exists():
            shutil.copy2(path, backup / label)
    connection = sqlite3.connect(database)
    try:
        connection.execute('PRAGMA busy_timeout = 5000')
        if connection.execute('PRAGMA integrity_check').fetchone()[0] != 'ok':
            raise RuntimeError('Orca database integrity check failed')
        with sqlite3.connect(backup / 'profile-state.db') as snapshot:
            connection.backup(snapshot)
        connection.execute('BEGIN IMMEDIATE')
        meta = dict(connection.execute('SELECT key, value FROM profile_state_meta'))
        if meta.get('profile_id') != 'local-default':
            raise RuntimeError('Unexpected Orca profile')
        row = connection.execute("SELECT payload, domain_version, content_hash FROM profile_state_documents WHERE domain = 'settings'").fetchone()
        if not row or row[1] != 1 or hashlib.sha256(row[0].encode()).hexdigest() != row[2]:
            raise RuntimeError('Unsupported or corrupt Orca settings document')
        previous = json.loads(row[0])
        updated = {**previous, **settings}
        if updated != previous:
            payload = json.dumps(updated, ensure_ascii=False, separators=(',', ':'))
            revision = int(meta['revision']) + 1
            connection.execute("UPDATE profile_state_documents SET payload=?, revision=?, updated_at=?, content_hash=? WHERE domain='settings'",
                               (payload, revision, int(time.time() * 1000), hashlib.sha256(payload.encode()).hexdigest()))
            connection.execute("UPDATE profile_state_meta SET value=? WHERE key='revision'", (str(revision),))
        connection.commit()
    finally:
        connection.close()
    write_json(bindings_path, bindings)
    asset = home / '.config/karabiner/assets/complex_modifications/orca-tmux-prefix.json'
    asset.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(rule_file, asset)
    write_json(karabiner_path, karabiner)
    print(f'Applied Orca settings, keybindings and Karabiner prefix. Backup: {backup}')


if __name__ == '__main__':
    os.umask(0o077)
    main()
