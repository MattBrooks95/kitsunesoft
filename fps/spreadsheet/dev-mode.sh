#!/usr/bin/env bash
tmux split-window -v 'npm run dev'
tmux split-window -v 'npm run server'
