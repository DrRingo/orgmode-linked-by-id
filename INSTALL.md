# 🚀 Hướng dẫn cài đặt nhanh

## Cài đặt trong 30 giây

### Bước 1: Thêm vào file config của bạn

Mở file config Emacs của bạn (thường là `~/.emacs`, `~/.emacs.d/init.el`, hoặc `~/.config/emacs/init.el`) và thêm dòng sau:

```elisp
(use-package orgmode-linked-by-id
  :straight (:type git :host github :repo "drringo/orgmode-linked-by-id")
  :config
  (require 'orgmode-linked-by-id))
```

### Bước 2: Restart Emacs

Khởi động lại Emacs để load package mới.

### Bước 3: Sử dụng

1. Tạo ID cho header: `SPC m I`
2. Tạo liên kết: `C-c l i` (trong file) hoặc `C-c l f` (trong thư mục)

## Các phương pháp cài đặt khác

### Nếu bạn dùng straight.el

```elisp
(straight-use-package
 '(orgmode-linked-by-id :type git :host github :repo "drringo/orgmode-linked-by-id"))
```

### Nếu bạn muốn cài đặt thủ công

```bash
git clone https://github.com/drringo/orgmode-linked-by-id.git ~/.emacs.d/orgmode-linked-by-id
```

Thêm vào config:
```elisp
(add-to-list 'load-path "~/.emacs.d/orgmode-linked-by-id")
(require 'orgmode-linked-by-id)
```

## Yêu cầu hệ thống

- Emacs 26.1+
- Org-mode
- Helm (cho fuzzy search)

## Phím tắt

| Phím tắt | Chức năng |
|----------|-----------|
| `C-c l i` | Liên kết header trong file hiện tại |
| `C-c l f` | Liên kết header trong thư mục |
| `C-c l c` | Liên kết hình ảnh/bảng có tên |

## Gặp vấn đề?

- Kiểm tra xem Helm đã được cài đặt chưa
- Đảm bảo Org-mode đã được load
- Restart Emacs sau khi cài đặt

## Hỗ trợ

- [GitHub Issues](https://github.com/drringo/orgmode-linked-by-id/issues)
- [GitHub Discussions](https://github.com/drringo/orgmode-linked-by-id/discussions) 